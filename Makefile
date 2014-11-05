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

clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump $(PARSER).erl
