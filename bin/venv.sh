#!/bin/bash

# set up virtualenvs for python2 and python3

set -e

VENV2=~/.emacs.d/python2-env
virtualenv $VENV2
$VENV2/bin/pip install -U pip
$VENV2/bin/pip install -U -r requirements.txt

VENV3=~/.emacs.d/python3-env
python3 -m venv $VENV3
$VENV3/bin/pip install -U pip
$VENV3/bin/pip install -U -r requirements.txt
