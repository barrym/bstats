REBAR=`which rebar || ./rebar`

all: compile

compile: deps
	@$(REBAR) compile

clean:
	@$(REBAR) clean

deps:
	@$(REBAR) get-deps

console:
	erl -pa ebin/ -pa deps/*/ebin/
