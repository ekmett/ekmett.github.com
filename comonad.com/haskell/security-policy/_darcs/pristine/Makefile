SETUP = ./Setup.lhs

all: build doc

.setup-config: security-policy.cabal 
	runhaskell $(SETUP) configure

configure: .setup-config

build: configure
	runhaskell $(SETUP) build

doc:
	runhaskell $(SETUP) haddock

sdist:
	runhaskell $(SETUP) sdist

test: build

clean:
	rm -rf dist 
	rm -f .setup-config .installed-pkg-config
	find src -name "*.hi" -or -name "*.o" -delete
