REBAR = rebar3 as test
all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

ct: compile app.config
	$(REBAR) ct

eunit: compile
	$(REBAR) eunit

xref:
	$(REBAR) xref

CUTTLEFISH_SCRIPT := _build/test/lib/cuttlefish/cuttlefish

app.config: $(CUTTLEFISH_SCRIPT) etc/emqx_web_hook.conf
	$(verbose) $(CUTTLEFISH_SCRIPT) -l info -e etc/ -c etc/emqx_web_hook.conf -i priv/emqx_web_hook.schema -d data

distclean:
	@rm -rf _build cover deps logs log data
	@rm -f rebar.lock compile_commands.json cuttlefish

