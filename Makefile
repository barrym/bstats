REBAR=`which rebar || ./rebar`

all: compile

compile: deps
	@$(REBAR) compile

clean:
	@$(REBAR) clean

deps:
	@$(REBAR) get-deps

console: compile
	erl -pa ebin/ -pa deps/*/ebin/ -eval 'application:start(bstats)'
