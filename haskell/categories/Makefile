URL=http://comonad.com/haskell/categories
TITLE="Category Transformer Library"

BASE=/home/slipwave/info/haskell/categories
SRC=$(BASE)/src

.PHONY: docs

build: all


all:
	@runhaskell Setup.lhs build

config:
	@runhaskell Setup.lhs configure

html:
	@runhaskell Setup.lhs haddock

install:
	@runhaskell Setup.lhs install

run:
	$(GHCI) Control.Category.Monad

test:
	find . -name "*.hs" | egrep -v '_darcs' | xargs qc

tags:
	find . -name "*.hs" | egrep -v '_darcs' | xargs hasktags -c
