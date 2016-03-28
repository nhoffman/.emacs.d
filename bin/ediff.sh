#!/bin/bash

if [[ -z $2 ]]; then
    echo "USAGE: ediff <FILE 1> <FILE 2>"
    exit 1
fi

if [[ $(uname) == 'Darwin' ]]; then
    EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
else
    EMACS=emacs
fi

TERM=xterm-256color ${EMACS:?} -nw -q \
    --eval "(setq ediff-split-window-function 'split-window-horizontally)" \
    --eval "(ediff-files \"$1\" \"$2\")"

rm -f $script
