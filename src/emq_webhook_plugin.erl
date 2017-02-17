%%%-------------------------------------------------------------------
%%% @author Sakib Sami
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2017 1:48 PM
%%%-------------------------------------------------------------------

-module(emq_webhook_plugin).
-author("Sakib Sami").

-behaviour(emqttd_auth_mod).

-include_lib("emqttd/include/emqttd.hrl").
-include_lib("emqttd/include/emqttd_cli.hrl").

-export([init/1, check/3, check_acl/2, reload_acl/1, description/0]).

init(Opts) ->
  ?PRINT_MSG("emq_webhook_plugin on init"),
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
