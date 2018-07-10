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

-module(emqx_web_hook_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:ensure_all_started(hackney),
    {ok, Sup} = emqx_web_hook_sup:start_link(),
    ok = hackney_pool:start_pool(emqx_pool, [{timeout, 5000}, {max_connections, 10000}]),
    emqx_web_hook:load(),
    emqx_web_hook_cfg:register(),
    {ok, Sup}.

stop(_State) ->
    hackney_pool:stop_pool(emqx_pool),
    application:stop(hackney),
    emqx_web_hook:unload(),
    emqx_web_hook_cfg:unregister(),
    ok.
