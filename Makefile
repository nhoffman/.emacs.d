CWD=$(shell pwd)

all:

submodules: ess magit

magit: FORCE
	cd magit; \
	git pull; \
	make clean; \
	make; \
	cd ${CWD}; \
	git add magit; \
	git commit -m "update magit"

ess: FORCE
	cd ess; \
	git pull; \
	make clean; \
	make ; \
	cd ${CWD}; \
	git add ess; \
	git commit -m "update ess"

xmodmap:
	cp .Xmodmap ${HOME}

FORCE:
