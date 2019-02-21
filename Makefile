PROJECT = emqx_web_hook
PROJECT_DESCRIPTION = EMQ X Webhook Plugin

DEPS = jsx clique
dep_jsx    = git-emqx https://github.com/talentdeficit/jsx v2.9.0
dep_clique = git-emqx https://github.com/emqx/clique v0.3.11

CUR_BRANCH := $(shell git branch | grep -e "^*" | cut -d' ' -f 2)
BRANCH := $(if $(filter $(CUR_BRANCH), master develop testing), $(CUR_BRANCH), testing)

BUILD_DEPS = emqx

dep_emqx = git-emqx https://github.com/emqx/emqx $(BRANCH)

ERLC_OPTS += +debug_info

TEST_ERLC_OPTS += +debug_info

COVER = true

$(shell [ -f erlang.mk ] || curl -s -o erlang.mk https://raw.githubusercontent.com/emqx/erlmk/master/erlang.mk)
include erlang.mk

CUTTLEFISH_SCRIPT = _build/default/lib/cuttlefish/cuttlefish

app.config: $(CUTTLEFISH_SCRIPT) etc/emqx_web_hook.conf
	$(verbose) $(CUTTLEFISH_SCRIPT) -l info -e etc/ -c etc/emqx_web_hook.conf -i priv/emqx_web_hook.schema -d data

$(CUTTLEFISH_SCRIPT): rebar-deps
	@if [ ! -f cuttlefish ]; then make -C _build/default/lib/cuttlefish; fi

distclean::
	@rm -rf _build cover deps logs log data
	@rm -f rebar.lock compile_commands.json cuttlefish

rebar-deps:
	rebar3 get-deps

rebar-clean:
	@rebar3 clean

rebar-compile: rebar-deps
	rebar3 compile

rebar-ct: app.config
	rebar3 ct

rebar-xref:
	@rebar3 xref
