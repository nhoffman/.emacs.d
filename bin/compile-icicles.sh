#!/bin/sh

emacs -q --no-site-file -batch \
		-eval "(add-to-list 'load-path \"icicles\")" \
		-f batch-byte-compile icicles/*.el
