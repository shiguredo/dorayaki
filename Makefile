.PHONY: all compile test clean distclean

all: clean deps test

deps:
	@./rebar3 update

compile:
	@./rebar3 do xref

test: compile
	@./rebar3 as test eunit

clean:
	@./rebar3 clean

distclean: clean
	@./rebar3 clean --all
