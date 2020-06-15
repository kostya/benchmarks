undefine GHC_PACKAGE_PATH

all:
	cabal update --builddir=target
	cabal build --builddir=target
	cabal install --overwrite-policy=always --installdir=../target/ --builddir=target

clean:
	-rm -rf target
