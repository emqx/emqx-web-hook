PROJECT = emqx_web_hook
PROJECT_DESCRIPTION = EMQ X Webhook Plugin
PROJECT_VERSION = 3.0

DEPS = jsx clique
dep_jsx    = git-emqx https://github.com/talentdeficit/jsx v2.9.0
dep_clique = git-emqx https://github.com/emqx/clique develop

BUILD_DEPS = emqx
dep_emqx = git-emqx https://github.com/emqx/emqx emqx30

ERLC_OPTS += +debug_info

TEST_ERLC_OPTS += +debug_info

COVER = true

define dep_fetch_git-emqx
	git clone -q --depth 1 -b $(call dep_commit,$(1)) -- $(call dep_repo,$(1)) $(DEPS_DIR)/$(call dep_name,$(1)) > /dev/null 2>&1; \
	cd $(DEPS_DIR)/$(call dep_name,$(1));
endef

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_web_hook.conf -i priv/emqx_web_hook.schema -d data

