build:
	cabal build

test: dist/build/spec/spec test/test_jobs/StubbornJob test/test_jobs/CompliantJob
	./dist/build/spec/spec


test/test_jobs/%: test/test_jobs/%.hs
	ghc --make $< -o $@

dist/build/spec/spec:
	cabal configure --enable-tests
	cabal build spec

.PHONY: test dist/build/spec/spec build
