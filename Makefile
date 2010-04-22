all: hell-fsck hell-insert hell-get hell-nodes hell-serve hell-meta hell-talk
lib = Hellnet.hs Hellnet/*.hs
execs = hell-fsck hell-insert hell-get hell-nodes hell-serve hell-meta
$(execs): %: %.hs $(lib)
	ghc --make -hide-package transformers $@
clean:
	find -name "*.hi" -exec rm {} \;
	find -name "*.o" -exec rm {} \;
jumpstart:
	cd ../hellage &&	ghc --make hellage && ghc --make hellage-genmeta && cp hellage{-genmeta,} ../hellnet
	tar c hell-{fsck,get,insert,meta,nodes,serve} hellage hellage-genmeta | bzip2 > hellnet-bin.tar.bz2
	scp hellnet-bin.tar.bz2 bitcheese.net:/var/www/dump.nblast.org/files/
	git describe
