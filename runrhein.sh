#!/usr/bin/env sh

BCTEMP=`mktemp tmp.bc.XXXXXX`
gosh -I src/rhc rhc $1 | gosh -I src/assembler vmasm > $BCTEMP

rheinvm $BCTEMP
rm $BCTEMP

