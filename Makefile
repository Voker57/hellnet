all: ${execs}
lib = Hellnet.hs Hellnet/*.hs
execs = hell-insert hell-get hell-nodes hell-serve hell-search hell-talk
$(execs): %: %.hs $(lib)
	ghc --make $@