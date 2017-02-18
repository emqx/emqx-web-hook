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

-behaviour(emqttd_auth_mod).

-include_lib("emqttd/include/emqttd.hrl").
-include_lib("emqttd/include/emqttd_cli.hrl").

-export([init/1, check/3, check_acl/2, reload_acl/1, description/0]).

init(Opts) ->
  lager:start(),
  lager:alert("emq_webhook_plugin [ Started ]"),
  mod_http:start(),
  {ok, Opts}.

check(#mqtt_client{client_id = ClientId, username = Username}, Password, _Opts) ->
  ?PRINT_MSG("emq_webhook_plugin on check"),
  ok.

check_acl({Client, PubSub, Topic}, Opts) ->
  ?PRINT_MSG("emq_webhook_plugin on check_acl"),
  allow.

reload_acl(_Opts) ->
  ok.

description() ->
  "EMQ Webhook Plugin".
