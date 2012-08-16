#!/bin/bash

ver=12.04-4

rm -rf ./ess && \
wget http://ess.r-project.org/downloads/ess/ess-${ver}.tgz && \
tar -xzf ess-${ver}.tgz && \
mv ess-${ver} ess && \
rm ess-${ver}.tgz
cd ess
make all
