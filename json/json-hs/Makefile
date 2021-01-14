undefine GHC_PACKAGE_PATH

build:
	~/.cabal/bin/hlint .
	cabal update -v0 --builddir=target
	cabal build -v0 --builddir=target
	cabal install -v0 --overwrite-policy=always --installdir=../target/ --builddir=target

clean:
	-rm -rf target
