CWD=$(shell pwd)

all:

publish:
	cd ../.emacs.d.ghpages && \
	git checkout gh-pages && \
	git commit -a -m "update ghpages" && \
	git push origin gh-pages
	cd ${CWD}
