all:
	pandoc spec.md --template=template.html --from=markdown_strict+yaml_metadata_block -s -o index.html

# -markdown_in_html_blocks +raw_html
