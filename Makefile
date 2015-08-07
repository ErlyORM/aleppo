ERL=erl
ERLC=erlc

PARSER=src/aleppo_parser

all: $(PARSER).erl
	-mkdir -p ebin
	$(ERL) -make 
	cp src/aleppo.app.src ebin/aleppo.app

$(PARSER).erl: $(PARSER).yrl
	$(ERLC) -o src/ src/aleppo_parser.yrl
 
run:
	$(ERL) -pa ebin

## dialyzer
PLT_FILE = ~/aleppo.plt
PLT_APPS ?= kernel stdlib erts
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions -Wunmatched_returns \
		-Wunderspecs --verbose --fullpath -n

.PHONY: dialyze
dialyze: all
	@[ -f $(PLT_FILE) ] || $(MAKE) plt
	@dialyzer --plt $(PLT_FILE) $(DIALYZER_OPTS) ebin || [ $$? -eq 2 ];

## In case you are missing a plt file for dialyzer,
## you can run/adapt this command
.PHONY: plt
plt:
	@echo "Building PLT, may take a few minutes"
	@dialyzer --build_plt --output_plt $(PLT_FILE) --apps \
		$(PLT_APPS) || [ $$? -eq 2 ];

clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump $(PARSER).erl
	rm -f $(PLT_FILE)
