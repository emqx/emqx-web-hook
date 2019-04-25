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
-define(RESOURCE_CONFIG_SPEC, #{
            url => #{type => string,
                     format => url,
                     required => true,
                     title => <<"Request URL">>,
                     description => <<"Request URL">>},
            headers => #{type => object,
                         schema => #{},
                         default => #{},
                         title => <<"Request Header">>,
                         description => <<"Request Header">>},
            method => #{type => string,
                        enum => [<<"PUT">>,<<"POST">>],
                        default => <<"POST">>,
                        title => <<"Request Method">>,
                        description => <<"Request Method">>}
        }).

-define(ACTION_PARAM_RESOURCE, #{
            type => string,
            required => true,
            title => <<"Resource ID">>,
            description => <<"Bind a resource to this action">>
        }).

-define(ACTION_MSG_SPEC, #{
            '$resource' => ?ACTION_PARAM_RESOURCE
        }).

-define(ACTION_EVENT_SPEC, #{
            '$resource' => ?ACTION_PARAM_RESOURCE,
            'template' => #{
                type => object,
                schema => #{},
                required => false,
                title => <<"Payload Template">>,
                description => <<"The payload template to be filled with variables before sending messages">>
            }
        }).

-define(JSON_REQ(URL, HEADERS, BODY), {(URL), (HEADERS), "application/json", (BODY)}).

-resource_type(#{name => ?RESOURCE_TYPE_WEBHOOK,
                 create => on_resource_create,
                 params => ?RESOURCE_CONFIG_SPEC,
                 description => "WebHook Resource"
                }).

-rule_action(#{name => publish_action,
               for => 'message.publish',
               func => forward_publish_action,
               params => ?ACTION_MSG_SPEC,
               type => ?RESOURCE_TYPE_WEBHOOK,
               description => "Forward Messages to Web Server"
              }).

-rule_action(#{name => event_action,
               for => '$events',
               func => forward_event_action,
               params => ?ACTION_EVENT_SPEC,
               type => ?RESOURCE_TYPE_WEBHOOK,
               description => "Forward Events to Web Server"
              }).

-type(action_fun() :: fun((Data :: map(), Envs :: map()) -> Result :: any())).

-type(url() :: binary()).

-export_type([action_fun/0]).

-export([on_resource_create/2]).

-export([ forward_publish_action/1
        , forward_event_action/1
        ]).

%%------------------------------------------------------------------------------
%% Actions for web hook
%%------------------------------------------------------------------------------

-spec(on_resource_create(binary(), map()) -> map()).
on_resource_create(_Name, Conf) ->
    Conf.

%% An action that forwards publish messages to a remote web server.
-spec(forward_publish_action(#{url() := string()}) -> action_fun()).
forward_publish_action(Params) ->
    #{url := Url, headers := Headers, method := Method}
        = parse_action_params(Params),
    fun(Selected, _Envs) ->
        http_request(Url, Headers, Method, Selected)
    end.

%% An action that forwards events to a remote web server.
-spec(forward_event_action(#{url() := string()}) -> action_fun()).
forward_event_action(Params) ->
    #{url := Url, headers := Headers, method := Method, template := Template}
        = parse_action_params(Params),
    fun(Selected, _Envs) ->
        http_request(Url, Headers, Method, feed_template(Template, Selected))
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

parse_action_params(Params = #{<<"url">> := Url}) ->
    try
        #{url => str(Url),
          headers => headers(maps:get(<<"headers">>, Params, undefined)),
          method => method(maps:get(<<"method">>, Params, <<"POST">>)),
          template => maps:get(<<"template">>, Params, undefined)}
    catch _:_ ->
        throw({invalid_params, Params})
    end.

method(GET) when GET == <<"GET">>; GET == <<"get">> -> get;
method(POST) when POST == <<"POST">>; POST == <<"post">> -> post;
method(PUT) when PUT == <<"PUT">>; PUT == <<"put">> -> put;
method(DEL) when DEL == <<"DELETE">>; DEL == <<"delete">> -> delete.

headers(undefined) -> [];
headers(Headers) when is_map(Headers) ->
    maps:fold(fun(K, V, Acc) ->
            [{str(K), str(V)} | Acc]
        end, [], Headers).

feed_template(undefined, Selected) ->
    maps:with([event, client_id, username], Selected);
feed_template(Template, Selected) when is_list(Template) ->
    [feed_template(T, Selected) || T <- Template];
feed_template(Template, Selected) when is_map(Template) ->
    maps:fold(
        fun(K, V, Acc) ->
            Acc#{K => feed_template(V, Selected)}
        end, #{}, Template);
feed_template(<<"${", Bin/binary>>, Selected) ->
    Val = binary:part(Bin, {0, byte_size(Bin)-1}),
    feed_val(Val, Selected);
feed_template(Bin, _Selected) ->
    Bin.

feed_val(Val, Selected) ->
    try V = binary_to_existing_atom(Val, utf8),
        maps:get(V, Selected, null)
    catch error:badarg ->
        null
    end.

str(Str) when is_list(Str) -> Str;
str(Atom) when is_atom(Atom) -> atom_to_list(Atom);
str(Bin) when is_binary(Bin) -> binary_to_list(Bin).
