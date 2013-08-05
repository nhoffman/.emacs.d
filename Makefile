CWD=$(shell pwd)

all:

tangle:
	#	emacs --script org-export/org-tangle.el -infile init.org
	which emacs

publish:
	cd ../.emacs.d.ghpages && \
	git checkout gh-pages && \
	git commit -a -m "update ghpages" && \
	git push origin gh-pages
	cd ${CWD}
