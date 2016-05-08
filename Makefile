all:
	ghc --make -O2 Main.hs -o Main
clean:
	-rm -f *.hi *.o Main
