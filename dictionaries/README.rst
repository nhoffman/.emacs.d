=======================
 Hunspell dictionaries
=======================

I've chosen to include dictionaries provided by the LibreOffice
project for use with hunspell in this repository. Dictionary files
were obtained from this url:
mnt/disk4/labs/hoffman/src/hunspell-1.6.0/en_US.aff

These files are distributed under the Mozilla Public License Version
2.0, which I have also included in this directory in ``LICENSE.txt``.

The environment variable ``DICPATH`` can be used to specify the path
of dictionary files. For example::

  DICPATH=~/.emacs.d/dictionaries hunspell
