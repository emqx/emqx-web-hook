PROJECT = emq_web_hook
PROJECT_DESCRIPTION = Web hook plugin
PROJECT_VERSION = 0.1

BUILD_DEPS = emqttd
dep_emqttd = git https://github.com/emqtt/emqttd master

COVER = true

include erlang.mk

app:: rebar.config
