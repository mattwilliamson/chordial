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
	$(ERL) $(SFLAGS) -noshell -eval 'systools:make_script("$(RELEASE)", [{outdir, "$(OUTDIR)"}, {path, ["$(OUTDIR)"]}])' -s erlang halt
	$(ERL) $(SFLAGS) -noshell -eval 'systools:script2boot("$(OUTDIR)/$(RELEASE)").' -s erlang halt

shell:
	$(ERL) $(SFLAGS)

clean:
	rm -rf $(OUTDIR)/*.beam
	rm -rf $(OUTDIR)/*.boot
	rm -rf $(OUTDIR)/*.script