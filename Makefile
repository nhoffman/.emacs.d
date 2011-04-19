home=${HOME}
thisdir=$(shell pwd)
version=$(shell svn info | grep Revision | cut -d' ' -f 2)

install:
	cp .Xmodmap ${home}

snapshot:
	cd .. ;\
	tar -czvf nh-dotemacs-${version}.gz \
		.emacs.d/init.el \
		.emacs.d/README.txt \
		.emacs.d/Makefile \
		.emacs.d/.Xmodmap \
		.emacs.d/*.el && \
	cp nh-dotemacs-${version}.gz nh-dotemacs.gz && \
	cd ${thisdir} 
