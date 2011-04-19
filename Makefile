CWD=$(shell pwd)

all:

extras: ess icicles

install: extras link xmodmap

ess:
	bin/get-ess.sh

icicles:
	bin/get-icicles.sh && bin/compile-icicles.sh

link:
	ln -sf ${CWD} ~/.emacs.d

xmodmap:
	cp .Xmodmap ${HOME}

snapshot:
	cd .. && \
	tar -czvf nh-dotemacs.tgz \
		.emacs.d/*.el \
		.emacs.d/README.rst \
		.emacs.d/Makefile \
		.emacs.d/.Xmodmap && \
	cd ${CWD}

