%%--------------------------------------------------------------------
%% Copyright (c) 2013-2018 EMQ Enterprise, Inc. (http://emqtt.io)
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
%%--------------------------------------------------------------------

-module(emqx_web_hook_rule).

-behaviour(gen_server).

-include_lib("emqx/include/emqx.hrl").

%% API
-export([start_link/0, insert/1, update/1, delete/1, forward/1,
         serialize/1, serialize/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TAB, mqtt_rule).

-define(SERVER, ?MODULE).

-record(rule, {id, type, enable, tenant_id, product_id, group_id, config}).

-record(state, {server}).

-import(proplists, [get_value/2]).

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

insert(Rule) when is_record(Rule, rule) ->
    gen_server:call(?SERVER, {insert, Rule}).

update(Rule) when is_record(Rule, rule) ->
    gen_server:call(?SERVER, {update, Rule}).

delete(Id) ->
    gen_server:call(?SERVER, {delete, Id}).

forward(Msg = #mqtt_message{topic=Topic, headers=Headers}) ->
    lager:debug("need forward message ~p", [Msg]),
    GroupId = get_value(group_id, Headers),
    [_, TenantId, _, ProductId | _T] = binary:split(Topic, <<"/">>, [global]),
    Rules = ets:match_object(?TAB, #rule{tenant_id=TenantId, enable=1, _='_', type=webhook}),
    [dispatch(Type, Msg, Rule)
      || Rule=#rule{group_id=GId, product_id=PId, config=Config, type=Type} <- Rules, (GId =:= GroupId orelse PId =:= ProductId)].


serialize(Id, Rule) when is_list(Rule) ->
    R = serialize(Rule), R#rule{id = Id}.

serialize(Rule) when is_list(Rule) ->
    Id        = get_value(<<"id">>, Rule),
    Type      = binary_to_atom(get_value(<<"ruleType">>, Rule), utf8),
    Enable    = get_value(<<"enable">>, Rule),
    TenantId  = get_value(<<"tenantID">>, Rule),
    ProductId = get_value(<<"productID">>, Rule),
    GroupId   = get_value(<<"groupID">>, Rule),
    Config    = config_(get_value(<<"config">>, Rule)),
    #rule{id=Id, type=Type, enable=Enable, tenant_id=TenantId,
          product_id=ProductId, group_id=GroupId, config=Config}.

config_(C) when is_list(C) ->
    C1 = lists:map(fun({K, V}) -> {binary_to_atom(K, utf8), V} end, C),
    maps:from_list(C1).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    %% 1. new ets table
    ets:new(?TAB, [named_table, public, {keypos, 2}]),
    gen_server:cast(?SERVER, get_all_rule),
    Server = application:get_env(emqx_web_hook, actor_server, ""),
    {ok, #state{server = Server}}.

handle_call({insert, Rule = #rule{id=Id}}, _From, State) ->
    case ets:lookup(?TAB, Id) of
        [_Rule] ->
            {reply, {error, has_existed}, State};
        [] ->
            ets:insert(?TAB, Rule),
            {reply, ok, State}
    end;

handle_call({update, Rule = #rule{id=Id}}, _From, State) ->
    case ets:lookup(?TAB, Id) of
        [] ->
            {reply, {error, not_exist}, State};
        [OldRule] ->
           ets:insert(?TAB, merge_record(OldRule, Rule)),
           {reply, ok, State}
    end;

handle_call({delete, Id}, _From, State) ->
    ets:delete(?TAB, Id),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(get_all_rule, State = #state{server=Server}) ->
    ets:insert(?TAB, request_all_rule(Server)),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%--------------------------------------------------------------------
%% Interval Funcs
%%--------------------------------------------------------------------

dispatch(webhook, Message, #rule{tenant_id=TId, product_id=PId, group_id=GId, config=#{url := Url}}) ->
    lager:debug("emqx_web_hook_rule dispatch forward message ~p to ~p", [Message, Url]),
    {FromClientId, _FromUsername} = format_from(Message#mqtt_message.from),
    [_, _, DeviceId] = binary:split(FromClientId, <<":">>, [global]),
    Params = [{deviceID, DeviceId},
              {productID, PId},
              {groupID, GId},
              {topic, Message#mqtt_message.topic},
              {qos, Message#mqtt_message.qos},
              {retain, Message#mqtt_message.retain},
              {payload, Message#mqtt_message.payload},
              {ts, emqx_time:now_secs(Message#mqtt_message.timestamp)}],
    send_http_request(binary_to_list(Url), Params);

dispatch(_Type, _Msg, _Rule) ->
    lager:error("emqx_web_hook_rule dispatch message failed, reason: not_supported_type"), ok.

request_all_rule(Url) ->
    case httpc:request(get, {Url, []}, [{timeout, 5000}], []) of
        {error, Reason} ->
            lager:error("get all rules error: ~p", [Reason]), [];
        {ok, {{_Ver, Code, _CodeDesc}, _Headers, Body}} ->
            lager:debug("request all rule return ~p, body ~p", [Code, Body]),
            case catch jsx:decode(list_to_binary(Body)) of
                {'EXIT', {badarg,_}} ->
                    lager:error("get all rules error, response not a json"), [];
                Rules ->
                    lists:map(fun serialize/1, Rules)
            end
    end.

format_from({ClientId, Username}) ->
    {ClientId, Username};
format_from(From) when is_atom(From) ->
    {a2b(From), a2b(From)};
format_from(_) ->
    {<<>>, <<>>}.

a2b(A) -> atom_to_binary(A, utf8).

merge_record(R1, R2) ->
    merge_record_(tuple_to_list(R1), tuple_to_list(R2)).

merge_record_([Name|P1], [Name|P2]) ->
    list_to_tuple(merge_record_(P1, P2, [Name])).

merge_record_([V1|T1], [undefined|T2], Acc) ->
    merge_record_(T1, T2, [V1|Acc]);

merge_record_([_V1|T1], [V2|T2], Acc) ->
    merge_record_(T1, T2, [V2|Acc]);

merge_record_([], [], Acc) -> lists:reverse(Acc).

send_http_request(Url, Params) ->
    Params1 = iolist_to_binary(mochijson2:encode(Params)),
    lager:debug("Url:~p, params:~s", [Url, Params1]),
    case httpc:request(post, {Url, [], "application/json", Params1}, [{timeout, 5000}], []) of
        {ok, _} -> ok;
        {error, Reason} ->
            lager:error("HTTP request error: ~p", [Reason]), ok %% TODO: return ok?
    end.

