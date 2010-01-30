all: hell-fsck hell-insert hell-get hell-nodes hell-serve hell-meta hell-talk
lib = Hellnet.hs Hellnet/*.hs
execs = hell-fsck hell-insert hell-get hell-nodes hell-serve hell-meta
$(execs): %: %.hs $(lib)
	ghc --make $@
clean:
	find -name "*.hi" -exec rm {} \;
	find -name "*.o" -exec rm {} \;
