URL=http://slipwave.info/haskell/jugs/
TITLE="Random Haskell Tools"

BASE=/home/slipwave/jugs
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
	$(GHCI) Js

test:
	find . -name "*.hs" | xargs qc

docs: 
	find . -name "*.hs" | egrep -v 'VHT|ref' | xargs haddock -html -odocs --source=$(URL) --title=$(TITLE)

tags:
	find . -name "*.hs" | xargs hasktags -c
