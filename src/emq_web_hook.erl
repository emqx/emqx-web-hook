%%--------------------------------------------------------------------
%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
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

-module(emq_web_hook).

-include_lib("emqttd/include/emqttd.hrl").

-define(APP, emq_web_hook).

-export([load/0, unload/0]).
-export([on_client_connected/3, on_client_disconnected/3]).
-export([on_client_subscribe/4, on_client_unsubscribe/4]).
-export([on_session_created/3, on_session_subscribed/4, on_session_unsubscribed/4, on_session_terminated/4]).
-export([on_message_publish/2, on_message_delivered/4, on_message_acked/4]).

load() ->
    RuleList = parse_rule(application:get_env(?APP, rules, [])),
    lists:foreach(fun({Hook, Fun, Filter}) ->
        load_(Hook, binary_to_atom(Fun, utf8), Filter, {Filter})
    end, RuleList),
    io:format("~s is loaded.~n", [?APP]), ok.

unload() ->
    RuleList = parse_rule(application:get_env(?APP, rules, [])),
    lists:foreach(fun({Hook, Fun, Filter}) ->
        unload_(Hook, binary_to_atom(Fun, utf8), Filter)
    end, RuleList).

%%--------------------------------------------------------------------
%% Client connected
%%--------------------------------------------------------------------
on_client_connected(ConnAck, Client = #mqtt_client{client_id = ClientId}, _Env) ->
    Params = [{action, client_connected}, 
              {client_id, ClientId},
              {conn_ack, ConnAck}],
    send_http_request(Params),
    {ok, Client}.

%%--------------------------------------------------------------------
%% Client disconnected
%%--------------------------------------------------------------------
on_client_disconnected({shutdown, Reason}, Client, Env) when is_atom(Reason) ->
    on_client_disconnected(Reason, Client, Env);
on_client_disconnected(Reason, _Client = #mqtt_client{client_id = ClientId}, _Env) when is_atom(Reason) ->
    Params = [{action, client_disconnected}, 
              {client_id, ClientId},
              {reason, Reason}],
    send_http_request(Params),
    ok;
on_client_disconnected(Reason, _Client, _Env) ->
    lager:error("Client disconnected reason:~p not encode json", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% Client subscribe
%%--------------------------------------------------------------------
on_client_subscribe(ClientId, Username, TopicTable, {Filter}) ->
    lists:foreach(fun({Topic, Opts}) -> 
        with_filter(
            fun() ->
                Params = [{action, client_subscribe}, 
                          {client_id, ClientId},
                          {username, Username},
                          {topic, Topic},
                          {opts, Opts}],
                send_http_request(Params)
            end, Topic, Filter)
    end, TopicTable),
    {ok, TopicTable}.

%%--------------------------------------------------------------------
%% Client unsubscribe
%%--------------------------------------------------------------------
on_client_unsubscribe(ClientId, Username, TopicTable, {Filter}) ->
    lists:foreach(fun({Topic, Opts}) -> 
        with_filter(
            fun() ->
                Params = [{action, client_unsubscribe}, 
                          {client_id, ClientId},
                          {username, Username},
                          {topic, Topic},
                          {opts, Opts}],
                send_http_request(Params)
            end, Topic, Filter)
    end, TopicTable),
    {ok, TopicTable}.
    

%%--------------------------------------------------------------------
%% Session created
%%--------------------------------------------------------------------
on_session_created(ClientId, Username, _Env) ->
    Params = [{action, session_created}, 
            {client_id, ClientId},
            {username, Username}],
    send_http_request(Params),
    ok.

%%--------------------------------------------------------------------
%% Session subscribed
%%--------------------------------------------------------------------
on_session_subscribed(ClientId, Username, {Topic, Opts}, {Filter}) ->
    with_filter(
        fun() ->
            Params = [{action, session_subscribed}, 
                      {client_id, ClientId},
                      {username, Username},
                      {topic, Topic},
                      {opts, Opts}],
            send_http_request(Params)
        end, Topic, Filter).

%%--------------------------------------------------------------------
%% Session unsubscribed
%%--------------------------------------------------------------------
on_session_unsubscribed(ClientId, Username, {Topic, _Opts}, {Filter}) ->
    with_filter(
        fun() ->
            Params = [{action, session_unsubscribed}, 
                      {client_id, ClientId},
                      {username, Username},
                      {topic, Topic}],
            send_http_request(Params)
        end, Topic, Filter).

%%--------------------------------------------------------------------
%% Session terminated
%%--------------------------------------------------------------------
on_session_terminated(ClientId, Username, {shutdown, Reason}, Env) when is_atom(Reason) ->
    on_session_terminated(ClientId, Username, Reason, Env);
on_session_terminated(ClientId, Username, Reason, _Env) when is_atom(Reason) ->
    Params = [{action, session_terminated}, 
            {client_id, ClientId},
            {username, Username},
            {reason, Reason}],
    send_http_request(Params),
    ok;
on_session_terminated(_ClientId, _Username, Reason, _Env) ->
    lager:error("Session terminated reason:~p not encode json", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% Message publish
%%--------------------------------------------------------------------
on_message_publish(Message = #mqtt_message{topic = <<"$SYS/", _/binary>>}, _Env) ->
  {ok, Message};
on_message_publish(Message = #mqtt_message{topic = Topic}, {Filter}) ->
    with_filter(
        fun()->
            {FromClientId, FromUsername} = format_from(Message#mqtt_message.from),
            Params = [{action, message_publish}, 
                      {from_client_id, FromClientId},
                      {from_username, FromUsername},
                      {topic, Message#mqtt_message.topic},
                      {qos, Message#mqtt_message.qos},
                      {retain, Message#mqtt_message.retain},
                      {payload, Message#mqtt_message.payload},
                      {ts, emqttd_time:now_secs(Message#mqtt_message.timestamp)}],
            send_http_request(Params),
            {ok, Message}
        end, Message, Topic, Filter).

%%--------------------------------------------------------------------
%% Message delivered
%%--------------------------------------------------------------------
on_message_delivered(ClientId, Username, Message = #mqtt_message{topic = Topic}, {Filter}) ->
    with_filter(
        fun()->
            {FromClientId, FromUsername} = format_from(Message#mqtt_message.from),
            Params = [{action, message_delivered},
                      {client_id, ClientId},
                      {username, Username},  
                      {from_client_id, FromClientId},
                      {from_username, FromUsername},
                      {topic, Message#mqtt_message.topic},
                      {qos, Message#mqtt_message.qos},
                      {retain, Message#mqtt_message.retain},
                      {payload, Message#mqtt_message.payload},
                      {ts, emqttd_time:now_secs(Message#mqtt_message.timestamp)}],
            send_http_request(Params)
        end, Topic, Filter).

%%--------------------------------------------------------------------
%% Message acked
%%--------------------------------------------------------------------
on_message_acked(ClientId, Username, Message = #mqtt_message{topic = Topic}, {Filter}) ->
    with_filter(
        fun()->
            {FromClientId, FromUsername} = format_from(Message#mqtt_message.from),  
            Params = [{action, message_acked}, 
                      {client_id, ClientId},
                      {username, Username}, 
                      {from_client_id, FromClientId},
                      {from_username, FromUsername},
                      {topic, Message#mqtt_message.topic},
                      {qos, Message#mqtt_message.qos},
                      {retain, Message#mqtt_message.retain},
                      {payload, Message#mqtt_message.payload},
                      {ts, emqttd_time:now_secs(Message#mqtt_message.timestamp)}],
            send_http_request(Params)
        end, Topic, Filter).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

send_http_request(Params) ->
    Params1 = list_to_binary(mochijson2:encode(Params)),
    Url = application:get_env(?APP, url, "http://127.0.0.1"),
    Method = post,
    Header = [],
    Type = "application/json",
    lager:debug("Url:~p, params:~p", [Url, Params1]),
    case httpc:request(Method, {Url, Header, Type, Params1}, [{timeout, 5000}], [])of
        {ok, _} -> ok;
        {error, Reason} -> throw({timeout, Reason})
    end.

parse_rule(Rules) ->
    parse_rule(Rules, []).
parse_rule([], Acc) ->
    lists:reverse(Acc);
parse_rule([{Rule, Conf} | Rules], Acc) ->
    {_, Params} = mochijson2:decode(Conf),
    Action = proplists:get_value(<<"action">>, Params),
    Filter = proplists:get_value(<<"topic">>, Params),
    parse_rule(Rules, [{list_to_atom(Rule), Action, Filter} | Acc]).


with_filter(Fun, _, undefined) ->
    Fun(), ok;
with_filter(Fun, Topic, Filter) ->
    case emqttd_topic:match(Topic, Filter) of
        true -> Fun(), ok;
        false -> ok
    end.

with_filter(Fun, _, _, undefined) ->
    Fun();
with_filter(Fun, Msg, Topic, Filter) ->
    case emqttd_topic:match(Topic, Filter) of
        true -> Fun();
        false -> {ok, Msg}
    end.

format_from({ClientId, Username}) ->
    {ClientId, Username};
format_from(From) when is_atom(From)->
    {a2b(From), a2b(From)};
format_from(_) -> 
    {<<>>, <<>>}.

a2b(A) -> erlang:atom_to_binary(A, utf8).

load_(Hook, Fun, Filter, Params) ->
    case Hook of
        'client.connected'     -> emqttd:hook(Hook, {Filter, fun ?MODULE:Fun/3}, [Params]);
        'client.disconnected'  -> emqttd:hook(Hook, {Filter, fun ?MODULE:Fun/3}, [Params]);
        'client.subscribe'     -> emqttd:hook(Hook, {Filter, fun ?MODULE:Fun/4}, [Params]);
        'client.unsubscribe'   -> emqttd:hook(Hook, {Filter, fun ?MODULE:Fun/4}, [Params]);
        'session.created'      -> emqttd:hook(Hook, {Filter, fun ?MODULE:Fun/3}, [Params]);
        'session.subscribed'   -> emqttd:hook(Hook, {Filter, fun ?MODULE:Fun/4}, [Params]);
        'session.unsubscribed' -> emqttd:hook(Hook, {Filter, fun ?MODULE:Fun/4}, [Params]);
        'session.terminated'   -> emqttd:hook(Hook, {Filter, fun ?MODULE:Fun/4}, [Params]); 
        'message.publish'      -> emqttd:hook(Hook, {Filter, fun ?MODULE:Fun/2}, [Params]);
        'message.acked'        -> emqttd:hook(Hook, {Filter, fun ?MODULE:Fun/4}, [Params]);
        'message.delivered'    -> emqttd:hook(Hook, {Filter, fun ?MODULE:Fun/4}, [Params])
    end.

unload_(Hook, Fun, Filter) ->
    case Hook of
        'client.connected'     -> emqttd:unhook(Hook, {Filter, fun ?MODULE:Fun/3});
        'client.disconnected'  -> emqttd:unhook(Hook, {Filter, fun ?MODULE:Fun/3});
        'client.subscribe'     -> emqttd:unhook(Hook, {Filter, fun ?MODULE:Fun/4});
        'client.unsubscribe'   -> emqttd:unhook(Hook, {Filter, fun ?MODULE:Fun/4});
        'session.created'      -> emqttd:unhook(Hook, {Filter, fun ?MODULE:Fun/3});
        'session.subscribed'   -> emqttd:unhook(Hook, {Filter, fun ?MODULE:Fun/4});
        'session.unsubscribed' -> emqttd:unhook(Hook, {Filter, fun ?MODULE:Fun/4});
        'session.terminated'   -> emqttd:unhook(Hook, {Filter, fun ?MODULE:Fun/4});
        'message.publish'      -> emqttd:unhook(Hook, {Filter, fun ?MODULE:Fun/2});
        'message.acked'        -> emqttd:unhook(Hook, {Filter, fun ?MODULE:Fun/4});
        'message.delivered'    -> emqttd:unhook(Hook, {Filter, fun ?MODULE:Fun/4})
    end.
