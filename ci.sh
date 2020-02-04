#!/bin/sh -eux

mirage configure -t $1 --prng fortuna
make depend
make
find . -type f -perm +1 -maxdepth 1 -exec sha256sum {} \;
