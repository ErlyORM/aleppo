REBAR=./rebar

## dialyzer
PLT_FILE = ~/aleppo.plt
PLT_APPS ?= kernel stdlib erts
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions -Wunmatched_returns \
		-Wunderspecs --verbose --fullpath -n

all: compile

compile:
	@$(REBAR) compile

run:
	@$(REBAR) shell

dialyze: all
	@[ -f $(PLT_FILE) ] || $(MAKE) plt
	@dialyzer --plt $(PLT_FILE) $(DIALYZER_OPTS) ebin || [ $$? -eq 2 ];

## In case you are missing a plt file for dialyzer,
## you can run/adapt this command
plt:
	@echo "Building PLT, may take a few minutes"
	@dialyzer --build_plt --output_plt $(PLT_FILE) --apps \
		$(PLT_APPS) || [ $$? -eq 2 ];

clean:
	@rm -fv erl_crash.dump
	@rm -f $(PLT_FILE)
	@$(REBAR) clean

.PHONY: all compile run dialyze plt clean
