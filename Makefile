PROJECT = emqx_web_hook
PROJECT_DESCRIPTION = EMQ X Webhook Plugin
PROJECT_VERSION = 3.0

DEPS = jsx clique
dep_jsx    = git https://github.com/talentdeficit/jsx v2.9.0
dep_clique = git https://github.com/emqx/clique

BUILD_DEPS = emqx
dep_emqx = git https://github.com/emqtt/emqttd emqx30

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'

TEST_ERLC_OPTS += +debug_info
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_web_hook.conf -i priv/emqx_web_hook.schema -d data

