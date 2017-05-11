all:
	pandoc spec.md --from markdown_strict -o index.html
