all: package

package: compile
	cat main.js | closure-compiler > main2.js
	mv main2.js ../static/main.js

compile:
	pulp browserify -O > main.js

dev: compile
	 cp main.js ../static/

clean:
	rm -rf .psc-package/ .pulp-cache/ .psci_modules/ bower_components/ output/
