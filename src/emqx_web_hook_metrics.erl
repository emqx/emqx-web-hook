%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(emqx_web_hook_metrics).

-behavior(gen_server).

-include_lib("emqx/include/logger.hrl").

-export([start_link/0]).

-export([ all/0
        , val/1
        , inc/1
        , inc/2
        , inc/3
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-define(CLIENT_METRICS, [
    {counter, 'client/connected'},
    {counter, 'client/disconnected'},
    {counter, 'client/subscribe'},
    {counter, 'client/unsubscribe'}
]).

-define(SESSION_METRICS, [
    {counter, 'session/created'},
    {counter, 'session/subscribed'},
    {counter, 'session/unsubscribed'},
    {counter, 'session/terminated'}
]).

-define(MESSAGE_METRICS, [
    {counter, 'message/publish'},
    {counter, 'message/deliver'},
    {counter, 'message/acked'}
]).

-define(TAB, ?MODULE).
-define(SERVER, ?MODULE).

%% @doc Start the metrics server.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% Metrics API
%%------------------------------------------------------------------------------

%% @doc Get all metrics
-spec(all() -> [{atom(), non_neg_integer()}]).
all() ->
    maps:to_list(
        ets:foldl(
            fun({{Metric, _N}, Val}, Map) ->
                    case maps:find(Metric, Map) of
                        {ok, Count} -> maps:put(Metric, Count+Val, Map);
                        error -> maps:put(Metric, Val, Map)
                    end
            end, #{}, ?TAB)).

%% @doc Get metric value
-spec(val(atom()) -> non_neg_integer()).
val(Metric) ->
    lists:sum(ets:select(?TAB, [{{{Metric, '_'}, '$1'}, [], ['$1']}])).

%% @doc Increase counter
inc(Metric) ->
    inc(counter, Metric, 1).

%% @doc Increase metric value
inc(Metric, Val) when is_atom(Metric) ->
    inc(counter, Metric, Val).

%% @doc Increase metric value
inc(Type, Metric, Val) ->
    update_counter(key(Type, Metric), {2, Val}).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

init([]) ->
    % Create metrics table
    _ = ets:new(?TAB, [public, set, named_table, {write_concurrency, true}]),
    lists:foreach(fun({counter, Name}) ->
                      Schedulers = lists:seq(1, emqx_vm:schedulers()),
                      ets:insert(?TAB, [{{Name, I}, 0} || I <- Schedulers])
                  end, ?CLIENT_METRICS ++ ?SESSION_METRICS ++ ?MESSAGE_METRICS),
    {ok, #{}, hibernate}.

handle_call(Req, _From, State) ->
    ?LOG(error, "[WebHook] Unexpected call: ~p", [Req]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    ?LOG(error, "[WebHook] Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?LOG(error, "[WebHook] Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #{}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

key(counter, Metric) ->
    {Metric, erlang:system_info(scheduler_id)}.

update_counter(Key, UpOp) ->
    ets:update_counter(?TAB, Key, UpOp).