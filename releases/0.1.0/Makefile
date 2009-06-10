ERL=erl
RELEASE=chordial
EBIN=../../ebin

all:
	$(ERL) $(SFLAGS) -noshell -eval 'systools:make_script("$(RELEASE)", [{path, ["$(EBIN)"]}])' -s erlang halt
	$(ERL) $(SFLAGS) -noshell -eval 'systools:script2boot("$(RELEASE)").' -s erlang halt
	$(ERL) $(SFLAGS) -noshell -eval 'systools:make_tar("$(RELEASE)", [{path, ["$(EBIN)"]}])' -s erlang halt
	
clean:
	rm -rf $(EBIN)/*.boot
	rm -rf $(EBIN)/*.script
	rm -rf $(EBIN)/*.tar.gz