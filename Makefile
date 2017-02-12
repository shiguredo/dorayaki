.PHONY: clean upgrade compile lint test dialyzer distclean circleci

all: clean deps lint test dialyzer

deps:
	@./rebar3 do update, upgrade

compile:
	@./rebar3 xref

lint:
	@./elvis rock --config elvis.config

test: compile
	@./rebar3 as test eunit

dialyzer:
	@./rebar3 dialyzer

clean:
	@./rebar3 clean

distclean: clean
	@./rebar3 clean --all
