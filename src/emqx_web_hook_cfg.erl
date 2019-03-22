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

-module(emqx_web_hook_cfg).

-export([ register/0
        , unregister/0
        ]).

-define(APP, emqx_web_hook).

register() ->
    clique_config:load_schema([code:priv_dir(?APP)], ?APP),
    register_config().

unregister() ->
    unregister_config(),
    clique_config:unload_schema(?APP).

register_config() ->
    Keys = keys(),
    [clique:register_config(Key , fun config_callback/2) || Key <- Keys],
    clique:register_config_whitelist(Keys, ?APP).

config_callback([_, _, _, Key], Value) ->
    application:set_env(?APP, list_to_atom(Key), Value),
    " successfully\n".

unregister_config() ->
    Keys = keys(),
    [clique:unregister_config(Key) || Key <- Keys],
    clique:unregister_config_whitelist(Keys, ?APP).

keys() ->
    ["web.hook.api.url"].
