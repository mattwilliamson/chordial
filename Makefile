ERLC=erlc
ERL=erl
FILES=src/*.erl
INCLUDEDIR=./include
OUTDIR=./ebin
EFLAGS=-I $(INCLUDEDIR) -o $(OUTDIR)
SFLAGS=-pa $(OUTDIR)

all:
	$(ERLC) $(EFLAGS) $(FILES)

shell:
	$(ERL) $(SFLAGS)

clean:
	rm $(OUTDIR)/*.beam
