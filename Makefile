RELEASE=chordial
ERLC=erlc
ERL=erl
SRCS=src/*.erl
INCLUDEDIR=./include
OUTDIR=./ebin
EFLAGS=-I $(INCLUDEDIR) -o $(OUTDIR)
SFLAGS=-pa $(OUTDIR)

all:
	$(ERLC) $(EFLAGS) $(SRCS)
	cd releases/0.1.0 && make

shell:
	$(ERL) $(SFLAGS)

clean:
	rm -rf $(OUTDIR)/*.beam
	cd releases/0.1.0 && make clean