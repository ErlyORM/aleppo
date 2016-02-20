REBAR=./rebar3
REBAR_VER = 3

## dialyzer

all: compile

compile:
	@$(REBAR) compile

run:
	@$(REBAR) shell

dialyze: all
	@$(REBAR) dialyzer

clean:
	@rm -fv erl_crash.dump
	@$(REBAR) clean

.PHONY: all compile run dialyze plt clean
