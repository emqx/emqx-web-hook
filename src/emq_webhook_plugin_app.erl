-module(emq_webhook_plugin_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  {ok, Sup} = emq_webhook_plugin_sup:start_link(),
  ok = emqttd_access_control:register_mod(auth, emq_webhook_plugin, []),
  ok = emqttd_access_control:register_mod(acl, emq_webhook_plugin, []),
  {ok, Sup}.

stop(_State) ->
  ok.
