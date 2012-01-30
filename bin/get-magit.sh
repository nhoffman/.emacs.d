#!/bin/bash

rm -rf magit
wget -O magit.tgz https://github.com/magit/magit/tarball/master
tar -xf magit.tgz
mv $(tar -tf magit.tgz 2> /dev/null | head -1) magit
rm magit.tgz
cd magit
make
