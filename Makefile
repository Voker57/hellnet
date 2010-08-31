all: $(execs)
lib = Hellnet.hs Hellnet/*.hs
execs = hell-fsck hell-insert hell-get hell-nodes hell-serve hell-meta hell-dir hell-serve-meta
$(execs): %: %.hs $(lib)
	ghc --make -hide-package transformers -hide-package monads-tf -hide-package monads-fd $@
clean:
	find -name "*.hi" -exec rm {} \;
	find -name "*.o" -exec rm {} \;
jumpstart:
	cd ../hellage &&	ghc --make hellage && ghc --make hellage-genmeta && cp hellage{-genmeta,} ../hellnet
	tar c hell-{fsck,get,insert,meta,nodes,serve,dir} hellage hellage-genmeta | bzip2 > hellnet-bin.tar.bz2
	scp hellnet-bin.tar.bz2 bitcheese.net:/var/www/dump.nblast.org/files/
	git describe
