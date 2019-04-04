%% Copyright (c) 2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

%% Define the default actions.
-module(emqx_web_hook_actions).

-include_lib("emqx/include/emqx.hrl").

-resource_type(#{name => 'web_hook',
                 schema => "emqx_web_hook.url",
                 create => on_resource_create,
                 description => "WebHook Resource"
                }).

-rule_action(#{name => forward_action,
               for => 'message.publish',
               func => forward_action,
               params => #{url => string},
               description => "Republish a MQTT message"
              }).

-type(action_fun() :: fun((Data :: map()) -> Result :: any())).

-export_type([action_fun/0]).

-export([on_resource_create/2]).
-export([forward_action/1]).

%%------------------------------------------------------------------------------
%% Actions for web hook
%%------------------------------------------------------------------------------

-spec(on_resource_create(binary(), map()) -> map()).
on_resource_create(_Name, Conf) ->
    Conf.

%% An action that forwards messages to a remote web server.
-spec(forward_action(#{url := string()}) -> action_fun()).
forward_action(#{url := Url}) ->
    fun(Msg = #{}) ->
        http_request(Url, Msg)
    end.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

http_request(Url, Params) ->
    logger:debug("[WebHook] HTTP request URL: ~s, params: ~p", [Url, Params]),
    case http_request(post, {str(Url), [], "application/json", jsx:encode(Params)},
                      [{timeout, 5000}], [], 0) of
        {ok, _} -> ok;
        {error, Reason} ->
            logger:error("[WebHook] HTTP request error: ~p", [Reason]),
            ok %% TODO: return ok?
    end.

http_request(Method, Req, HTTPOpts, Opts, Times) ->
    %% Resend request, when TCP closed by remotely
    case httpc:request(Method, Req, HTTPOpts, Opts) of
        {error, socket_closed_remotely} when Times < 3 ->
            timer:sleep(trunc(math:pow(10, Times))),
            http_request(Method, Req, HTTPOpts, Opts, Times+1);
        Other -> Other
    end.

str(Str) when is_list(Str) -> Str;
str(Bin) when is_binary(Bin) -> binary_to_list(Bin).