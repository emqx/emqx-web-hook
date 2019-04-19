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

-define(RESOURCE_TYPE_WEBHOOK, 'web_hook').
-define(RESOURCE_CONFIG_SPEC,
        #{url => #{type => url,
                   required => true,
                   description => <<"Request URL">>},
          headers => #{type => object,
                       required => false,
                       default => #{},
                       description => <<"Request Header">>},
          method => #{type => enum,
                      enum => ['GET','PUT','POST','DELETE'],
                      required => false,
                      default => 'POST',
                      description => <<"Request Method">>}}).

-define(JSON_REQ(URL, HEADERS, BODY), {(URL), (HEADERS), "application/json", (BODY)}).

-resource_type(#{name => ?RESOURCE_TYPE_WEBHOOK,
                 create => on_resource_create,
                 params => ?RESOURCE_CONFIG_SPEC,
                 description => "WebHook Resource"
                }).

-rule_action(#{name => publish_action,
               for => 'message.publish',
               func => forward_publish_action,
               params => #{'$resource' => ?RESOURCE_TYPE_WEBHOOK},
               type => ?RESOURCE_TYPE_WEBHOOK,
               description => "Forward Messages to Web Server"
              }).

-rule_action(#{name => event_action,
               for => '$events',
               func => forward_event_action,
               params => #{'$resource' => ?RESOURCE_TYPE_WEBHOOK,
                           template => json},
               type => ?RESOURCE_TYPE_WEBHOOK,
               description => "Forward Events to Web Server"
              }).

-type(action_fun() :: fun((Data :: map(), Envs :: map()) -> Result :: any())).

-export_type([action_fun/0]).

-export([on_resource_create/2]).

-export([ forward_publish_action/1
        , forward_event_action/1
        ]).

-export([feed_template/2]).

%%------------------------------------------------------------------------------
%% Actions for web hook
%%------------------------------------------------------------------------------

-spec(on_resource_create(binary(), map()) -> map()).
on_resource_create(_Name, Conf) ->
    validate_resource_config(Conf, ?RESOURCE_CONFIG_SPEC),
    Conf.

%% An action that forwards publish messages to a remote web server.
-spec(forward_publish_action(#{url := string()}) -> action_fun()).
forward_publish_action(Params) ->
    #{url := Url, headers := Headers, method := Method}
        = parse_action_params(Params),
    fun(Selected, _Envs) ->
        http_request(Url, Headers, Method, Selected)
    end.

%% An action that forwards events to a remote web server.
-spec(forward_event_action(#{url := string()}) -> action_fun()).
forward_event_action(Params) ->
    #{url := Url, headers := Headers, method := Method, template := Template}
        = parse_action_params(Params),
    fun(_Selected, Envs) ->
        http_request(Url, Headers, Method, feed_template(Template, Envs))
    end.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

http_request(Url, Headers, Method, Params) ->
    logger:debug("[WebHook Action] ~s to ~s, headers: ~s, body: ~p", [Method, Url, Headers, Params]),
    case http_request(Method, ?JSON_REQ(Url, Headers, jsx:encode(Params)),
                      [{timeout, 5000}], [], 0) of
        {ok, _} -> ok;
        {error, Reason} ->
            logger:error("[WebHook Action] HTTP request error: ~p", [Reason])
    end.

http_request(Method, Req, HTTPOpts, Opts, Times) ->
    %% Resend request, when TCP closed by remotely
    case httpc:request(Method, Req, HTTPOpts, Opts) of
        {error, socket_closed_remotely} when Times < 3 ->
            timer:sleep(trunc(math:pow(10, Times))),
            http_request(Method, Req, HTTPOpts, Opts, Times+1);
        Other -> Other
    end.

validate_resource_config(_Config, _ConfigSepc) ->
    %% erlang:error(invaild_config)
    ok.

parse_action_params(Params = #{url := Url}) ->
    #{url => str(Url),
      headers => headers(maps:get(headers, Params, undefined)),
      method => method(maps:get(method, Params, <<"POST">>)),
      template => maps:get(template, Params, undefined)}.

method(GET) when GET == <<"GET">>; GET == <<"get">> -> get;
method(POST) when POST == <<"POST">>; POST == <<"post">> -> post;
method(PUT) when PUT == <<"PUT">>; PUT == <<"put">> -> put;
method(DEL) when DEL == <<"DELETE">>; DEL == <<"delete">> -> delete.

headers(undefined) -> [];
headers(Headers) ->
    [{str(K), str(V)} || {K, V} <- Headers].

feed_template(undefined, Envs) ->
    maps:with([event, client_id, username], Envs);
feed_template(Template, Envs) when is_list(Template) ->
    lists:foldr(
        fun({K, V}, Acc) ->
               [{K, feed_template(V, Envs)} | Acc];
           (V, Acc) ->
               [feed_template(V, Envs) | Acc]
        end, [], Template);
feed_template(<<"${", Bin/binary>>, Envs) ->
    Val = binary:part(Bin, {0, byte_size(Bin)-1}),
    feed_val(Val, Envs);
feed_template(Bin, _Envs) ->
    Bin.

feed_val(Val, Envs) ->
    try V = binary_to_existing_atom(Val, utf8),
        maps:get(V, Envs, null)
    catch error:badarg ->
        null
    end.

str(Str) when is_list(Str) -> Str;
str(Bin) when is_binary(Bin) -> binary_to_list(Bin).
