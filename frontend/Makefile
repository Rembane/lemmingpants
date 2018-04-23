all: package

.PHONY: dev compile package all styles clean

package: compile
	cat main.js | closure-compiler > main2.js
	mv main2.js ../static/main.js

compile: styles
	pulp browserify -O > main.js

dev: compile
	 cp main.js ../static/

styles:
	sassc -t compressed -I styles/ styles/main.scss > ../static/bundle.css

clean:
	rm -rf .psc-package/ .pulp-cache/ .psci_modules/ bower_components/ output/
