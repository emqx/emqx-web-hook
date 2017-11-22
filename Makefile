PROJECT = emqx_web_hook
PROJECT_DESCRIPTION = EMQ X Webhook Plugin
PROJECT_VERSION = 2.3.0

DEPS = clique
dep_clique  = git https://github.com/emqtt/clique

BUILD_DEPS = emqx
dep_emqx = git git@github.com:emqx/emqx-enterprise

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'

TEST_DEPS = emqttc
dep_emqttc = git https://github.com/emqtt/emqttc

TEST_ERLC_OPTS += +debug_info
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_web_hook.conf -i priv/emqx_web_hook.schema -d data

