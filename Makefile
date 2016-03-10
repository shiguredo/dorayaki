.PHONY: all compile test clean distclean

APP_NAME = dora

all: clean deps test

deps:
	@./rebar3 compile

compile:
	@./rebar3 do xref

test: compile
	@./rebar3 as test eunit

clean:
	@./rebar3 clean

distclean: clean
	rm -rf dev
	@./rebar3 clean --all

dialyze-init:
	dialyzer --build_plt --apps erts kernel stdlib mnesia crypto public_key snmp reltool
	dialyzer --add_to_plt --plt ~/.dialyzer_plt --output_plt $(APP_NAME).plt -c .
	dialyzer -c ebin -Wunmatched_returns -Werror_handling -Wrace_conditions -Wunderspecs

dialyze: compile
	dialyzer --check_plt --plt $(APP_NAME).plt -c .
	dialyzer -c ebin
