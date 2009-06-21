all: hell-insert hell-get hell-nodes hell-serve hell-search
lib = Hellnet.hs Hellnet/*.hs
execs = hell-insert hell-get hell-nodes hell-serve hell-search
$(execs): %: %.hs $(lib)
	ghc --make $@