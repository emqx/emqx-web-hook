%% Copyright (c) 2018 EMQ Technologies Co., Ltd. All Rights Reserved.
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
     {emqx_web_hook_actions, [sequence], [case1]}
    ].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    [start_apps(App, DataDir) || App <- [emqx, emqx_web_hook]],
    start_http_(),
    Config.

end_per_suite(_Config) ->
    http_server:stop_http(),
    [application:stop(App) || App <- [emqx_web_hook, emqx]].

reload(_Config) ->
    {ok, Rules} = application:get_env(emqx_web_hook, rules),
    emqx_web_hook:unload(),
    timer:sleep(10),
    lists:foreach(fun({HookName, _Action}) ->
                          ?assertEqual([], ?HOOK_LOOKUP(HookName))
                  end, Rules),
    emqx_web_hook:load(),
    lists:foreach(fun({HookName, _Action}) ->
                          Hooks  = ?HOOK_LOOKUP(HookName),
                          ?assertEqual(true, length(Hooks) > 0)
                  end, Rules).

server_config(_) ->
    emqx_cli_config:run(["config", "set", "web.hook.api.url=https://example.com", "--app=emqx_web_hook"]),
    {ok, Url} =  application:get_env(emqx_web_hook, url),
    ?assertEqual("https://example.com", Url).

change_config(_Config) ->
    {ok, Rules} = application:get_env(emqx_web_hook, rules),
    emqx_web_hook:unload(),
    HookRules = lists:keydelete("message.delivered", 1, Rules),
    application:set_env(emqx_web_hook, rules, HookRules),
    emqx_web_hook:load(),
    ?assertEqual([], ?HOOK_LOOKUP("message.delivered")),
    emqx_web_hook:unload(),
    application:set_env(emqx_web_hook, rules, Rules),
    emqx_web_hook:load().

case1(_Config) ->
    {ok, C, _} = emqx_client:start_link([{host, "localhost"}, {client_id, <<"simpleClient">>}, {username, <<"username">>}]),
    emqx_client:subscribe(C, <<"TopicA">>, qos2),
    emqx_client:publish(C, <<"TopicA">>, <<"Payload...">>, qos2),
    emqx_client:unsubscribe(C, <<"TopicA">>),
    emqx_client:disconnect(C),
    ok.

start_apps(App, DataDir) ->
    Schema = cuttlefish_schema:files([filename:join([DataDir, atom_to_list(App) ++ ".schema"])]),
    Conf = conf_parse:file(filename:join([DataDir, atom_to_list(App) ++ ".conf"])),
    NewConfig = cuttlefish_generator:map(Schema, Conf),
    Vals = proplists:get_value(App, NewConfig),
    [application:set_env(App, Par, Value) || {Par, Value} <- Vals],
    application:ensure_all_started(App).

hooks_(HookName) ->
    string:join(lists:append(["on"], string:tokens(HookName, ".")), "_").

start_http_() ->
    http_server:start_http().
