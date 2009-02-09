all:
	cd lib/chordial && make all

shell:
	make all
	cd lib/chordial && make shell

clean:
	cd lib/chordial && make clean
