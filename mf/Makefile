default: all

all: haskell 

dev: all
	cd src && ghci Dev

#----------------------------

src/Lexer.hs: src/Lexer.x
	cd src && alex Lexer.x

src/Parser.hs: src/Parser.y
	cd src && happy Parser.y

# uuagc:
#  -H              --haskellsyntax                 Use Haskell like syntax (equivalent to --lckeywords and --doublecolons --genlinepragmas)
#  -d              --data                          generate data type definition
#  -c              --catas                         generate catamorphisms
#  -f              --semfuns                       generate semantic functions
#  -w              --wrappers                      generate wappers for semantic domains
#  -s              --signatures                    generate signatures for semantic functions
src/AttributeGrammar.hs: src/AttributeGrammar.ag src/AG/Program.ag src/AG/MonotoneFramework.ag
	cd src && uuagc -Hdcfws --self AttributeGrammar.ag --module AttributeGrammar

#----------------------------

generate: src/AttributeGrammar.hs src/Lexer.hs src/Parser.hs

# cabal build will detect changes in source files itself
haskell: generate
	cabal build

clean:
	rm -f src/AttributeGrammar.hs src/Parser.hs src/Lexer.hs src/*.hi src/*.o src/Main

.PHONY: default all clean generate haskell
