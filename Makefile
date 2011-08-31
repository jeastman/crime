ERL ?= erl
APP = crime

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

test: all
	@./rebar skip_deps=true eunit

start: app
	exec erl -pa $(PWD)/apps/*/ebin -pa $(PWD)/deps/*/ebin -boot start_sasl -s reloader -s crime_web -s crime_core
