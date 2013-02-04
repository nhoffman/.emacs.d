CWD=$(shell pwd)

all:

publish:
	cd ../.emacs.d.ghpages; \
	git commit -a -m "update ghpages"; \
	git push origin gh-pages;
	cd ${CWD}
