all: hell-fsck hell-insert hell-get hell-nodes hell-serve hell-search hell-talk
lib = Hellnet.hs Hellnet/*.hs
execs = hell-fsck hell-insert hell-get hell-nodes hell-serve
$(execs): %: %.hs $(lib)
	ghc --make $@
clean:
	find -name "*.hi" -exec rm {} \;
	find -name "*.o" -exec rm {} \;