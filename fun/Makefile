CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS     :=

all : haskell

src/CCO/HM/AG.hs : src/CCO/HM/AG.ag src/CCO/HM/AG/Base.ag
	uuagc -Hdcfws --self -P src/CCO/HM src/CCO/HM/AG.ag

src/CCO/SystemF/AG.hs : src/CCO/SystemF/AG.ag src/CCO/SystemF/AG/Base.ag \
		src/CCO/SystemF/AG/Printing.ag
	uuagc -Hdcfws --self -P src/CCO/SystemF src/CCO/SystemF/AG.ag

src/CCO/Core/AG.hs : src/CCO/Core/AG.ag src/CCO/Core/AG/Base.ag \
		src/CCO/Core/AG/ToCoreRun.ag
	uuagc -Hdcfws --self -P src/CCO/Core src/CCO/Core/AG.ag

haskell : src/CCO/Core/AG.hs src/CCO/HM/AG.hs src/CCO/SystemF/AG.hs
	cabal configure $(CABAL-CONFIGURE-FLAGS)
	cabal build $(CABAL-BUILD-FLAGS)

.PHONY : haskell
