CABAL = cabal-dev

default: build

build: configure
	$(CABAL) build

configure: angel.cabal install_dependencies
	$(CABAL) configure

configure_tests: angel.cabal install_test_dependencies
	$(CABAL) configure --enable-tests

install_test_dependencies: angel.cabal
	$(CABAL) install --enable-tests --only-dependencies 

install_dependencies: angel.cabal
	$(CABAL) install --only-dependencies

spec: configure_tests
	PATH=$$PATH:cabal-dev/bin $(CABAL) build > /dev/null
	$(CABAL) test
