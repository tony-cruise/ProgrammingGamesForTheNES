#! /bin/bash

set -e

rm -f example.o
rm -f example.nes
rm -f example.map.txt
rm -f example.labels.txt
rm -f example.nes.*
rm -f example.dbg

echo Compiling...
ca65 example.s -g -o example.o

echo Linking...
ld65 -o example.nes -C example.cfg example.o -m example.map.txt -Ln example.labels.txt --dbgfile example.dbg
echo Success!
