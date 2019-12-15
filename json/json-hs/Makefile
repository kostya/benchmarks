undefine GHC_PACKAGE_PATH

all:
	cabal update
	cabal build
	cabal install --overwrite-policy=always --installdir=../
