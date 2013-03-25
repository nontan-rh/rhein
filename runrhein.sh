#!/usr/bin/env sh

BCTEMP=`mktemp tmp.bc.XXXXXX`
gosh -I src/compiler rheinc $1 | gosh -I src/assembler vmasm > $BCTEMP

rheinvm $BCTEMP
rm $BCTEMP

