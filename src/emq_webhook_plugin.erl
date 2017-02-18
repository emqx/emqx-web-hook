%%%--------------------------------------------------------------------------------
%% The MIT License (MIT)
%%
%% Copyright (c) 2017 Coders Garage Technologies Ltd
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/ or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%%--------------------------------------------------------------------------------

-module(emq_webhook_plugin).

-include_lib("emqttd/include/emqttd.hrl").

-define(APP, emq_webhook_plugin).
-define(API_URL, "api_url").
-define(API_KEY, "api_key").

-export([load/1, unload/0]).
-export([on_client_connected/3, on_client_disconnected/3]).
-export([on_client_subscribe/4, on_client_unsubscribe/4]).
-export([on_session_created/3, on_session_subscribed/4, on_session_unsubscribed/4, on_session_terminated/4]).
-export([on_message_publish/2, on_message_delivered/4, on_message_acked/4]).

load(Env) ->
  emqttd:hook('client.connected', fun ?MODULE:on_client_connected/3, [Env]),
  emqttd:hook('client.disconnected', fun ?MODULE:on_client_disconnected/3, [Env]),
  emqttd:hook('client.subscribe', fun ?MODULE:on_client_subscribe/4, [Env]),
  emqttd:hook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/4, [Env]),
  emqttd:hook('session.created', fun ?MODULE:on_session_created/3, [Env]),
  emqttd:hook('session.subscribed', fun ?MODULE:on_session_subscribed/4, [Env]),
  emqttd:hook('session.unsubscribed', fun ?MODULE:on_session_unsubscribed/4, [Env]),
  emqttd:hook('session.terminated', fun ?MODULE:on_session_terminated/4, [Env]),
  emqttd:hook('message.publish', fun ?MODULE:on_message_publish/2, [Env]),
  emqttd:hook('message.delivered', fun ?MODULE:on_message_delivered/4, [Env]),
  emqttd:hook('message.acked', fun ?MODULE:on_message_acked/4, [Env]).

on_client_connected(ConnAck, Client = #mqtt_client{client_id = ClientId}, _Env) ->
  Params = "{\"action\": \"client_connected\", " ++ "\"clientId\": \"" ++ binary_to_list(ClientId) ++ "\",
  \"api_key\": \"" ++ get_config(?API_KEY, _Env) ++ "\"}",
  mod_http:send(get_config(?API_URL, _Env), list_to_binary(Params)),
  {ok, Client}.

on_client_disconnected(Reason, _Client = #mqtt_client{client_id = ClientId}, _Env) ->
  Params = "{\"action\": \"client_disconnected\", " ++ "\"clientId\": \"" ++ binary_to_list(ClientId) ++ "\",
  \"api_key\": \"" ++ get_config(?API_KEY, _Env) ++ "\"}",
  mod_http:send(get_config(?API_URL, _Env), list_to_binary(Params)),
  ok.

on_client_subscribe(ClientId, Username, TopicTable, _Env) ->
  Params = "{\"action\": \"client_subscribe\", " ++ "\"clientId\": \"" ++ binary_to_list(ClientId) ++ "\",
  \"username\": \"" ++ binary_to_list(Username) ++ "\", \"api_key\": \"" ++ get_config(?API_KEY, _Env) ++ "\"}",
  mod_http:send(get_config(?API_URL, _Env), list_to_binary(Params)),
  {ok, TopicTable}.

on_client_unsubscribe(ClientId, Username, TopicTable, _Env) ->
  Params = "{\"action\": \"client_unsubscribe\", " ++ "\"clientId\": \"" ++ binary_to_list(ClientId) ++ "\",
  \"username\": \"" ++ binary_to_list(Username) ++ "\", \"api_key\": \"" ++ get_config(?API_KEY, _Env) ++ "\"}",
  mod_http:send(get_config(?API_URL, _Env), list_to_binary(Params)),
  {ok, TopicTable}.

on_session_created(ClientId, Username, _Env) ->
  Params = "{\"action\": \"session_created\", " ++ "\"clientId\": \"" ++ binary_to_list(ClientId) ++ "\",
  \"username\": \"" ++ binary_to_list(Username) ++ "\", \"api_key\": \"" ++ get_config(?API_KEY, _Env) ++ "\"}",
  mod_http:send(get_config(?API_URL, _Env), list_to_binary(Params)),
  ok.

on_session_subscribed(ClientId, Username, {Topic, Opts}, _Env) ->
  Params = "{\"action\": \"session_subscribed\", " ++ "\"clientId\": \"" ++ binary_to_list(ClientId) ++ "\",
  \"username\": \"" ++ binary_to_list(Username) ++ "\", \"topic\": \"" ++ binary_to_list(Topic) ++ "\",
  \"api_key\": \"" ++ get_config(?API_KEY, _Env) ++ "\"}",
  mod_http:send(get_config(?API_URL, _Env), list_to_binary(Params)),
  {ok, {Topic, Opts}}.

on_session_unsubscribed(ClientId, Username, {Topic, Opts}, _Env) ->
  Params = "{\"action\": \"session_unsubscribed\", " ++ "\"clientId\": \"" ++ binary_to_list(ClientId) ++ "\",
  \"username\": \"" ++ binary_to_list(Username) ++ "\", \"topic\": \"" ++ binary_to_list(Topic) ++ "\",
  \"api_key\": \"" ++ get_config(?API_KEY, _Env) ++ "\"}",
  mod_http:send(get_config(?API_URL, _Env), list_to_binary(Params)),
  ok.

on_session_terminated(ClientId, Username, Reason, _Env) ->
  Params = "{\"action\": \"session_terminated\", " ++ "\"clientId\": \"" ++ binary_to_list(ClientId) ++ "\",
  \"username\": \"" ++ binary_to_list(Username) ++ "\", \"reason\": \"" ++ binary_to_list(Reason) ++ "\",
  \"api_key\": \"" ++ get_config(?API_KEY, _Env) ++ "\"}",
  mod_http:send(get_config(?API_URL, _Env), list_to_binary(Params)),
  ok.

on_message_publish(Message = #mqtt_message{topic = <<"$SYS/", _/binary>>}, _Env) ->
  {ok, Message};

on_message_publish(Message, _Env) ->
  Params = "{\"action\": \"message_publish\", " ++ "\"topic\": \"" ++ binary_to_list(Message#mqtt_message.topic) ++ "\",
  \"message\": \"" ++ binary_to_list(Message#mqtt_message.payload) ++ "\", \"from\": \"" ++ tuple_to_list(Message#mqtt_message.from) ++ "\",
  \"api_key\": \"" ++ get_config(?API_KEY, _Env) ++ "\"}",
  mod_http:send(get_config(?API_URL, _Env), list_to_binary(Params)),
  {ok, Message}.

on_message_delivered(ClientId, Username, Message, _Env) ->
  Params = "{\"action\": \"message_delivered\", " ++ "\"topic\": \"" ++ binary_to_list(Message#mqtt_message.topic) ++ "\",
  \"message\": \"" ++ binary_to_list(Message#mqtt_message.payload) ++ "\", \"from\": \"" ++ binary_to_list(Message#mqtt_message.from) ++ "\",
  \"username\": \"" ++ binary_to_list(Username) ++ "\", \"api_key\": \"" ++ get_config(?API_KEY, _Env) ++ "\"}",
  mod_http:send(get_config(?API_URL, _Env), list_to_binary(Params)),
  {ok, Message}.

on_message_acked(ClientId, Username, Message, _Env) ->
  Params = "{\"action\": \"message_acknowledged\", " ++ "\"topic\": \"" ++ binary_to_list(Message#mqtt_message.topic) ++ "\",
  \"message\": \"" ++ binary_to_list(Message#mqtt_message.payload) ++ "\", \"from\": \"" ++ binary_to_list(Message#mqtt_message.from) ++ "\",
  \"username\": \"" ++ binary_to_list(Username) ++ "\", \"api_key\": \"" ++ get_config(?API_KEY, _Env) ++ "\"}",
  mod_http:send(get_config(?API_URL, _Env), list_to_binary(Params)),
  {ok, Message}.

get_config(Lookup, Configs) ->
  [ConfigHead | ConfigTail] = Configs,
  KeyValuePair = tuple_to_list(ConfigHead),
  Key = lists:nth(1, KeyValuePair),
  if
    Key == Lookup -> lists:nth(2, KeyValuePair);
    true ->
      if
        length(ConfigTail) == 0 -> undefined;
        true ->
          get_config(Lookup, ConfigTail)
      end
  end.

unload() ->
  emqttd:unhook('client.connected', fun ?MODULE:on_client_connected/3),
  emqttd:unhook('client.disconnected', fun ?MODULE:on_client_disconnected/3),
  emqttd:unhook('client.subscribe', fun ?MODULE:on_client_subscribe/4),
  emqttd:unhook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/4),
  emqttd:unhook('session.subscribed', fun ?MODULE:on_session_subscribed/4),
  emqttd:unhook('session.unsubscribed', fun ?MODULE:on_session_unsubscribed/4),
  emqttd:unhook('message.publish', fun ?MODULE:on_message_publish/2),
  emqttd:unhook('message.delivered', fun ?MODULE:on_message_delivered/4),
  emqttd:unhook('message.acked', fun ?MODULE:on_message_acked/4).
