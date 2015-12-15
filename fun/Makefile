CABAL_CONFIGURE_FLAGS := $(CABAL_CONFIGURE_FLAGS) --user

all : haskell

src/lib/CCO/HM/AG.hs : src/lib/CCO/HM/AG.ag src/lib/CCO/HM/AG/Base.ag src/lib/CCO/HM/AG/HMToCR.ag src/lib/CCO/HM/AG/ToANF.ag
	uuagc -Hdcfws --self -P src/lib/CCO/HM src/lib/CCO/HM/AG.ag

src/lib/CCO/SystemF/AG.hs : src/lib/CCO/SystemF/AG.ag src/lib/CCO/SystemF/AG/Base.ag \
		src/lib/CCO/SystemF/AG/Printing.ag
	uuagc -Hdcfws --self -P src/lib/CCO/SystemF src/lib/CCO/SystemF/AG.ag

src/lib/CCO/Core/AG.hs : src/lib/CCO/Core/AG.ag src/lib/CCO/Core/AG/Base.ag \
		src/lib/CCO/Core/AG/ToCoreRun.ag
	uuagc -Hdcfws --self -P src/lib/CCO/Core src/lib/CCO/Core/AG.ag

haskell : src/lib/CCO/Core/AG.hs src/lib/CCO/HM/AG.hs src/lib/CCO/SystemF/AG.hs
	cabal configure $(CABAL_CONFIGURE_FLAGS)
	cabal build $(CABAL_BUILD_FLAGS)

clean:
	cabal clean
	rm -f "src/lib/CCO/HM/AG.hs" "src/lib/CCO/SystemF/AG.hs" "src/lib/CCO/Core/AG.hs"

.PHONY : all haskell clean prof
