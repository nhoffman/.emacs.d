#!/bin/bash

set -e

PYTHON=${PYTHON-python}
VENV=emacs-env
VENV_VER=1.11.6
VENV_URL="https://pypi.python.org/packages/source/v/virtualenv"
SCONS_VER=2.3.2

if [[ -n "$VIRTUAL_ENV" ]]; then
    echo "Error: virtualenv $VIRTUAL_ENV is already active"
fi

check_version(){
    # usage: check_version module version-string
    "$PYTHON" <<EOF 2> /dev/null
import $1
from distutils.version import LooseVersion
assert LooseVersion($1.__version__) >= LooseVersion("$2")
EOF
}

# create virtualenv if necessary
if [[ ! -f "${VENV:?}/bin/activate" ]]; then
    # if the system virtualenv is up to date, use it
    if check_version virtualenv $VENV_VER; then
	echo "using $(which virtualenv) (version $(virtualenv --version))"
    	virtualenv "$VENV"
    else
	echo "downloading virtualenv version $VENV_VER"
	if [[ ! -f src/virtualenv-${VENV_VER}/virtualenv.py ]]; then
	    mkdir -p src
	    (cd src && \
		wget -N ${VENV_URL}/virtualenv-${VENV_VER}.tar.gz && \
		tar -xf virtualenv-${VENV_VER}.tar.gz)
	fi
	"$PYTHON" src/virtualenv-${VENV_VER}/virtualenv.py "$VENV"
    fi
else
    echo "virtualenv $VENV already exists"
fi

source $VENV/bin/activate
pip install --upgrade -r requirements.txt

# scons can't be installed using pip
if ! which scons > /dev/null; then
    (cd src && \
	wget -N http://downloads.sourceforge.net/project/scons/scons/${SCONS_VER}/scons-${SCONS_VER}.tar.gz && \
	tar -xf scons-${SCONS_VER}.tar.gz && \
	cd scons-${SCONS_VER} && \
	"$PYTHON" setup.py install)
else
    echo "scons is already installed in $(which scons)"
fi

echo "packages installed in ${VIRTUAL_ENV}:"
pip freeze

