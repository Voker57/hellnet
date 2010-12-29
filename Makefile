all: $(execs)
lib = Hellnet.hs Hellnet/*.hs
execs = hell-fsck hell-insert hell-get hell-nodes hell-serve hell-meta hell-dir hell-serve-meta
$(execs): %: %.hs $(lib)
	ghc --make $@
clean:
	find -name "*.hi" -exec rm {} \;
	find -name "*.o" -exec rm {} \;
jumpstart:
	tar c hell-{fsck,get,insert,meta,nodes,serve,dir} | bzip2 > hellnet-bin.tar.bz2
	scp hellnet-bin.tar.bz2 bitcheese.net:/var/www/dump.nblast.org/files/
	git describe
