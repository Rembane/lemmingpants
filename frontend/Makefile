all:
	pulp browserify -O | closure-compiler > main.js

dev: all
	cp main.js ../static/
