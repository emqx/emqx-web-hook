%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-emqx_plugin(?MODULE).

-include("emqx_web_hook.hrl").

-export([ start/2
        , stop/1
        , translate_env/0
        , inet/1
        , ssl/1
        ]).

start(_StartType, _StartArgs) ->
    translate_env(),
    {ok, Sup} = emqx_web_hook_sup:start_link(),
    {ok, PoolOpts} = application:get_env(?APP, pool_opts),
    ehttpc_sup:start_pool(?APP, ssl(inet(PoolOpts))),
    emqx_web_hook:register_metrics(),
    emqx_web_hook:load(),
    {ok, Sup}.

stop(_State) ->
    emqx_web_hook:unload(),
    ehttpc_sup:stop_pool(?APP).

translate_env() ->
    {ok, URL} = application:get_env(?APP, url),
    #{host := Host0,
      port := Port,
      path := Path} = uri_string:parse(list_to_binary(URL)),
    {ok, Host} = inet:parse_address(binary_to_list(Host0)),
    application:set_env(?APP, path, Path),
    PoolOpts = application:get_env(?APP, pool_opts, []),
    application:set_env(?APP, pool_opts, [{host, Host}, {port, Port} | PoolOpts]).

inet(PoolOpts) ->
    case proplists:get_value(host, PoolOpts) of
        Host when tuple_size(Host) =:= 8 ->
            TransOpts = proplists:get_value(transport_opts, PoolOpts, []),
            NewPoolOpts = proplists:delete(transport_opts, PoolOpts),
            [{transport_opts, [inet6 | TransOpts]} | NewPoolOpts];
        _ ->
            PoolOpts
    end.

ssl(PoolOpts) ->
    case proplists:get_value(ssl, PoolOpts, []) of
        [] ->
            PoolOpts;
        SSLOpts ->
            TransOpts = proplists:get_value(transport_opts, PoolOpts, []),
            NewPoolOpts = proplists:delete(transport_opts, PoolOpts),
            [{transport_opts, SSLOpts ++ TransOpts}, {transport, ssl} | NewPoolOpts]
    end.