all:
	ghc --make Capp.hs

clean:
	-rm -f *.hi *.o
