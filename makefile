carl: FORCE
	@echo "Compiling"
	cabal configure --user --enable-tests
	cabal build
	cabal test
	cabal install

FORCE:
