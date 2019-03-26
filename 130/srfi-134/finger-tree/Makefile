
doc: finger-tree.scm
	chibi-doc --html finger-tree.scrbl > finger-tree.html

clean:
	rm -f finger-tree.html

test: finger-tree.sld finger-tree.scm finger-tree-test.scm
	chibi-scheme finger-tree-test.scm
