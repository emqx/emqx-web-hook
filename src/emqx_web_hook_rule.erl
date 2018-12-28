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

-include("emqx_web_hook.hrl").

%% API
-export([start_link/0, insert/1, insert/2, update/1, update/2, delete/1, delete/2, forward/1,
         serialize/1, serialize/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TAB, mqtt_rule).

-define(SERVER, ?MODULE).

-define(WEBHOOK, 1).

-define(APICLOUD, 2).

-define(LEANCLOUD, 3).

-record(state, {server}).

-import(proplists, [get_value/2]).

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

insert(Rule) when is_record(Rule, rule) ->
    [insert(Node, Rule) || Node <- ekka_mnesia:running_nodes()].

insert(Node, Rule) when Node =:= node() ->
    gen_server:call(?SERVER, {insert, Rule});
insert(Node, Rule) ->
    rpc_call(Node, insert, [Rule]).

update(Rule) when is_record(Rule, rule) ->
    [update(Node, Rule) || Node <- ekka_mnesia:running_nodes()].

update(Node, Rule) when Node =:= node() ->
    gen_server:call(?SERVER, {update, Rule});
update(Node, Rule) ->
    rpc_call(Node, update, [Rule]).

delete(Id) ->
    [delete(Node, Id) || Node <- ekka_mnesia:running_nodes()].

delete(Node, Id) when Node =:= node() ->
    gen_server:call(?SERVER, {delete, Id});
delete(Node, Id) ->
    rpc_call(Node, delete, [Id]).

forward(Msg = #mqtt_message{topic=_Topic, headers=Headers}) ->
    lager:debug("emqx_web_hook_rule check forward message ~p", [Msg]),
    GroupId   = get_value(group_id, Headers),
    TenantId  = get_value(tenant_id, Headers),
    ProductId = get_value(product_id, Headers),

    Rules = ets:match_object(?TAB, #rule{tenant_id=TenantId, enable=1, _='_', type=webhook}),
    [dispatch(Type, Msg, Rule)
     || Rule=#rule{group_id=GId, product_id=PId, config=_Config, type=Type} <- Rules, PId =:= ProductId].

serialize(Id, Rule) when is_list(Rule) ->
    R = serialize(Rule), R#rule{id = Id}.

serialize(Rule) when is_list(Rule) ->
    Id        = get_value(<<"id">>, Rule),
    Type      = rule_type(get_value(<<"ruleType">>, Rule)),
    Enable    = get_value(<<"enable">>, Rule),
    TenantId  = get_value(<<"tenantID">>, Rule),
    ProductId = get_value(<<"productID">>, Rule),
    GroupId   = get_value(<<"groupID">>, Rule),
    Config    = config_(get_value(<<"config">>, Rule)),
    #rule{id=Id, type=Type, enable=Enable, tenant_id=TenantId,
          product_id=ProductId, group_id=GroupId, config=Config}.

rule_type(1) -> webhook.

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

handle_call({insert, Rule = #rule{id=Id, tenant_id = TenantId}}, _From, State) ->
    case ets:lookup(?TAB, Id) of
        [_Rule] ->
            {reply, {error, has_existed}, State};
        [] ->
            ets:insert(?TAB, Rule),
            emqx_web_hook:load(<<"+/", TenantId/binary, "/#">>),
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
    case ets:lookup(?TAB, Id) of
        [#rule{tenant_id = TenantId}] ->
            ets:delete(?TAB, Id),
            emqx_web_hook:unload(<<"+/", TenantId/binary, "/#">>),
            ok;
        [] ->
            ok
    end,
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(get_all_rule, State = #state{server=Server}) ->
    lists:foreach(fun(Rule = #rule{tenant_id = TenantId}) ->
        ets:insert(?TAB, Rule),
        emqx_web_hook:load(<<"up/+/", TenantId/binary, "/#">>),
        ok
    end, request_all_rule(Server)),
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

dispatch(webhook, Message, #rule{tenant_id=_TId, product_id=PId, group_id=GId, config=Conf}) ->
    #{url := Url, type := Type} = Conf,
    lager:debug("emqx_web_hook_rule dispatch forward ~p message ~p to ~p", [Type, Message, Url]),
    {FromClientId, _FromUsername} = format_from(Message#mqtt_message.from),
    case binary:split(FromClientId, <<":">>, [global]) of
        [_,_,DeviceId] ->
            Params = [{deviceID, DeviceId},
                      {productID, PId},
                      {groupID, GId},
                      {topic, unmount(Message#mqtt_message.topic)},
                      {qos, Message#mqtt_message.qos},
                      {retain, Message#mqtt_message.retain},
                      {payload, Message#mqtt_message.payload},
                      {ts, emqx_time:now_secs(Message#mqtt_message.timestamp)}],
            send_http_request(Url, authorization(Conf, Type), Params);
        _ ->
            lager:error("emqx_web_hook_rule dispatch failed, unexpected clientid ~p", [FromClientId])
    end;

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

unmount(Topic) ->
    %% Topic = <<"tenants/CSX9Rh2lL/products/P3gKub/test">>
    [_, _, _, _, _|Topic1] = binary:split(Topic, <<"/">>, [global]),
    iolist_to_binary(lists:join("/", Topic1)).

authorization(#{token := Token, type := Type}, ?WEBHOOK) ->
    [{<<"Authorization">>, iolist_to_binary([Type, " ", Token])}];
authorization(#{appKey := AppKey, appId := AppId}, ?APICLOUD) ->
    TimeNow = integer_to_list(emqx_time:now_ms()),
    [{<<"X-APICloud-AppId">>, AppId},
     {<<"X-APICloud-AppKey">>, erlang:list_to_binary(
                                 string:to_lower(
                                   emqx_web_hook_sha1:hexstring(binary_to_list(AppId)++
                                                                    "UZ"++binary_to_list(AppKey)++
                                                                    "UZ"++TimeNow)
                                   ++"."++TimeNow)
                                )
     }];
authorization(#{appKey := AppKey, appId := AppId}, ?LEANCLOUD) ->
    [{<<"X-LC-Id">>, AppId},
     {<<"X-LC-Key">>, AppKey}];
authorization(_Conf,_Type) ->
    [].

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

send_http_request(Url, Headers, Params) ->
    Params1 = iolist_to_binary(mochijson2:encode(Params)),
    lager:debug("send http request url: ~p, header: ~p, params: ~p", [Url, Headers, Params1]),
    emqx_web_hook:http_request(post, Params1, Url, Headers).

rpc_call(Node, Fun, Args) ->
    case rpc:call(Node, ?MODULE, Fun, Args) of
        {badrpc, Reason} -> {error, Reason};
        Res -> Res
    end.