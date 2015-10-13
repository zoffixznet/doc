.PHONY: html html-nohighlight test help

html:
	perl6 htmlify.p6

html-nohighlight:
	perl6 htmlify.p6 --no-highlight

sparse:
	perl6 htmlify.p6 --no-highlight --sparse=10

test:
	prove --exec perl6 -r t

help:
	@echo "Usage: make [html|html-nohighlight|test|sparse|run]"
	@echo ""
	@echo "Options:"
	@echo "   html:             generate the HTML documentation"
	@echo "   html-nohighlight: generate HTML documentation without syntax highlighting"
	@echo "   test:             run the test suite"
	@echo "   sparse:           generate a subset of the HTML documentation"
	@echo "   run:              serve out html/ directory"

run:
	@echo "Starting local server…"
	perl app.pl daemon
