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

-module(emqx_web_hook_SUITE).

-compile(export_all).

-include_lib("emqx/include/emqx.hrl").

-include_lib("common_test/include/ct.hrl").

-include_lib("eunit/include/eunit.hrl").

-type(hooktag() :: atom() | string() | binary()).

-record(callback, {tag            :: hooktag(),
                   function       :: function(),
                   init_args = [] :: list(any()),
                   priority  = 0  :: integer()}).

-define(HOOK_LOOKUP(H),         emqx_hooks:lookup(list_to_atom(H))).

all() ->
    [{group, emqx_web_hook_actions},
     {group, emqx_web_hook}].

groups() ->
    [{emqx_web_hook, [sequence], [reload, change_config]},
     {emqx_web_hook_actions, [sequence], [validate_web_hook]}
    ].

init_per_suite(Config) ->
    emqx_ct_helpers:start_apps([emqx, emqx_web_hook]),
    Config.

end_per_suite(_Config) ->
    emqx_ct_helpers:stop_apps([emqx, emqx_web_hook]).

reload(_Config) ->
    {ok, Rules} = application:get_env(emqx_web_hook, rules),
    lists:foreach(fun({HookName, _Action}) ->
                          Hooks  = ?HOOK_LOOKUP(HookName),
                          ?assertEqual(true, length(Hooks) > 0)
                  end, Rules).

change_config(_Config) ->
    {ok, Rules} = application:get_env(emqx_web_hook, rules),
    emqx_web_hook:unload(),
    HookRules = lists:keydelete("message.deliver", 1, Rules),
    application:set_env(emqx_web_hook, rules, HookRules),
    emqx_web_hook:load(),
    ?assertEqual([], ?HOOK_LOOKUP("message.deliver")),
    emqx_web_hook:unload(),
    application:set_env(emqx_web_hook, rules, Rules),
    emqx_web_hook:load().

validate_web_hook(_Config) ->
    http_server:start_http(),
    {ok, C} = emqx_client:start_link([{host, "localhost"}, {client_id, <<"simpleClient">>}, {username, <<"username">>}]),
    {ok, _} = emqx_client:connect(C),
    emqx_client:subscribe(C, <<"TopicA">>, qos2),
    emqx_client:publish(C, <<"TopicA">>, <<"Payload...">>, qos2),
    emqx_client:unsubscribe(C, <<"TopicA">>),
    emqx_client:disconnect(C),
    ValidateData = get_http_message(self()),
    [validate_http_data(A) || A <- ValidateData],
    http_server:stop_http(),
    ok.

hooks_(HookName) ->
    string:join(lists:append(["on"], string:tokens(HookName, ".")), "_").

get_http_message(Pid) ->
    {messages, MailboxMessage} = erlang:process_info(Pid, messages),
    [maps:from_list(jsx:decode(Info)) || [{Info, _}] <- MailboxMessage].

validate_http_data(#{<<"action">> := <<"session_created">>,<<"client_id">> := ClientId, <<"username">> := Username}) ->
    ?assertEqual(<<"simpleClient">>, ClientId),
    ?assertEqual(<<"username">>, Username);
validate_http_data(#{<<"action">> := <<"client_connected">>,<<"client_id">> := ClientId, <<"username">> := Username}) ->
    ?assertEqual(<<"simpleClient">>, ClientId),
    ?assertEqual(<<"username">>, Username);
validate_http_data(#{<<"action">> := <<"client_subscribe">>,<<"client_id">> := ClientId, <<"topic">> := Topic,
                     <<"username">> := Username}) ->
    ?assertEqual(<<"simpleClient">>, ClientId),
    ?assertEqual(<<"username">>, Username),
    ?assertEqual(<<"TopicA">>, Topic);
validate_http_data(#{<<"action">> := <<"session_subscribed">>, <<"client_id">> := ClientId, <<"topic">> := Topic}) ->
    ?assertEqual(<<"simpleClient">>, ClientId),
    ?assertEqual(<<"TopicA">>, Topic);
validate_http_data(#{<<"action">> := <<"message_publish">>, <<"from_client_id">> := ClientId,
                     <<"from_username">> := Username, <<"payload">> := Payload,<<"qos">> := Qos,
                     <<"retain">> := Retain, <<"topic">> := Topic}) ->
    ?assertEqual(<<"Payload...">>, Payload),
    ?assertEqual(2, Qos),
    ?assertEqual(false, Retain),
    ?assertEqual(<<"simpleClient">>, ClientId),
    ?assertEqual(<<"username">>, Username),
    ?assertEqual(<<"TopicA">>, Topic); 
validate_http_data(#{<<"action">> := <<"message_deliver">>, <<"client_id">> := ClientId,
                     <<"from_client_id">> := FromClientId,<<"from_username">> := FromUsername,
                     <<"payload">> := Payload,<<"qos">> := Qos,<<"retain">> := Retain,<<"topic">> := Topic,
                     <<"username">> := Username})->
    ?assertEqual(<<"Payload...">>, Payload),
    ?assertEqual(2, Qos),
    ?assertEqual(false, Retain),
    ?assertEqual(<<"simpleClient">>, ClientId),
    ?assertEqual(<<"username">>, Username),
    ?assertEqual(<<"TopicA">>, Topic),
    ?assertEqual(<<"simpleClient">>, FromClientId),
    ?assertEqual(<<"username">>, FromUsername);
validate_http_data(#{<<"action">> := <<"client_unsubscribe">>, <<"client_id">> := ClientId,
                     <<"topic">> := Topic,<<"username">> := Username}) ->
    ?assertEqual(<<"TopicA">>, Topic),
    ?assertEqual(<<"username">>, Username),
    ?assertEqual(<<"simpleClient">>, ClientId);

validate_http_data(#{<<"action">> := <<"session_unsubscribed">>,
                     <<"client_id">> := ClientId, <<"topic">> := Topic}) ->
    ?assertEqual(<<"TopicA">>, Topic),
    ?assertEqual(<<"simpleClient">>, ClientId);
validate_http_data(#{<<"action">> := <<"message_acked">>, <<"client_id">> := ClientId,
                    <<"from_client_id">> := FromClietId, <<"from_username">> := FromUsername,
                    <<"payload">> := Payload,<<"qos">> := Qos,<<"retain">> := false,<<"topic">> := TopicA}) ->
    ?assertEqual(<<"simpleClient">>, ClientId),
    ?assertEqual(<<"simpleClient">>, FromClietId),
    ?assertEqual(<<"username">>, FromUsername),
    ?assertEqual(<<"Payload...">>, Payload),
    ?assertEqual(2, Qos),
    ?assertEqual(<<"TopicA">>, TopicA);
validate_http_data(_ValidateData) ->
    ct:fail("fail").