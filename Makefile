all: hell-insert hell-get hell-nodes hell-serve # hell-get hell-nodes hell-serve
lib = Hellnet.hs Hellnet/*.hs
execs = hell-insert hell-get hell-nodes hell-serve
$(execs): %: %.hs $(lib)
	ghc --make $@