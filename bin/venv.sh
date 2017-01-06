#!/bin/bash

set -e

VENV=emacs-env
virtualenv $VENV
source $VENV/bin/activate
pip install -U pip
pip install -U -r requirements.txt

