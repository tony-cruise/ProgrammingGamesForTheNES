#! /bin/bash

set -e

rm -f megablast.o
rm -f megablast.nes
rm -f megablast.map.txt
rm -f megablast.labels.txt
rm -f megablast.nes.*
rm -f megablast.dbg

echo Compiling...
ca65 megablast.s -g -o megablast.o

echo Linking...
ld65 -o megablast.nes -C megablast.cfg megablast.o -m megablast.map.txt -Ln megablast.labels.txt --dbgfile megablast.dbg
echo Success!
