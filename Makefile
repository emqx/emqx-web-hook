PROJECT = emqx_web_hook
PROJECT_DESCRIPTION = EMQ X Webhook Plugin

CUR_BRANCH := $(shell git branch | grep -e "^*" | cut -d' ' -f 2)
BRANCH := $(if $(filter $(CUR_BRANCH), master develop), $(CUR_BRANCH), develop)

DEPS = jsx clique emqx_rule_engine
dep_jsx    = git-emqx https://github.com/talentdeficit/jsx v2.9.0
dep_clique = git-emqx https://github.com/emqx/clique v0.3.11
dep_emqx_rule_engine = git-emqx https://github.com/emqx/emqx-rule-engine $(BRANCH)

BUILD_DEPS = emqx

dep_emqx = git-emqx https://github.com/emqx/emqx $(BRANCH)

TEST_DEPS = emqx_ct_helper
dep_emqx_ct_helper = git-emqx https://github.com/emqx/emqx-ct-helpers v1.0

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
