ERLC=erlc
ERL=erl
FILES=src/*.erl
INCLUDEDIR=./include
OUTDIR=./ebin
CFLAGS=-I $(INCLUDEDIR) -o $(OUTDIR)
SFLAGS=-pa $(OUTDIR)

all:
	$(ERLC) $(CFLAGS) $(FILES)

shell:
	$(ERL) $(SFLAGS)

clean:
	rm $(OUTDIR)/*.beam
