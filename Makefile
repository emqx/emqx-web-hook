PROJECT = emq_webhook_plugin
PROJECT_DESCRIPTION = EMQ Webhook Plugin
PROJECT_VERSION = 0.1

BUILD_DEPS = emqttd
dep_emqttd = git https://github.com/emqtt/emqttd master

COVER = true

include erlang.mk

app:: rebar.config
