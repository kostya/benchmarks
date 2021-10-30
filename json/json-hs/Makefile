undefine GHC_PACKAGE_PATH

build:
	~/.cabal/bin/hlint .
	cabal update --builddir=target
	cabal build --builddir=target
	cabal install --overwrite-policy=always --installdir=../target/ --builddir=target

clean:
	-rm -rf target
