all: package

.PHONY: dev compile package all styles clean test start-anew

DIRS = $(shell find output/ -name '*.js')

start-anew: clean package
	true

package: compile
	cat main.js | closure-compiler > main2.js
	mv main2.js ../static/main.js

compile: styles install-dependencies
	@echo "Compiling..."
	@purs bundle ${DIRS} -o main.js --main Main -m Main

dev: compile
	 cp main.js ../static/

styles:
	sassc -t compressed -I styles/ styles/main.scss > ../static/bundle.css

install-dependencies:
	psc-package install
	psc-package build

build-tests: install-dependencies
	@echo "Compiling..."
	@psc-package build test/Main.purs

test: build-tests
	@purs bundle ${DIRS} --output test.js --main Test.Main --module Test.Main
	cat test.js | node

clean:
	rm -rf .psc-package/ .pulp-cache/ .psci_modules/ bower_components/ output/
