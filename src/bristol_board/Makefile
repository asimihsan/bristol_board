ERL ?= erl
APP = csd

.PHONY: deps

all: deps
	@./rebar compile

app:
	@./rebar compile skip_deps=true

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

webstart: app
	exec erl -pa $(PWD)/apps/*/ebin -pa $(PWD)/deps/*/ebin -boot start_sasl -s reloader -s bb_web -s bb_core -s bb_database +K true +A 5 -name bristol_board@127.0.0.1 -env ERL_MAX_PORTS 4096 -env ERL_FULLSWEEP_AFTER 10 -heart

