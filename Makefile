compile:
	@./rebar compile

clean:
	@./rebar clean

test:
	@./rebar skip_deps=true eunit

.PHONY: compile clean test
