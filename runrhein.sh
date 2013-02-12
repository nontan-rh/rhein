#!/usr/bin/env sh

PYTHONPATH="$PYTHONPATH:." gosh -I src/compiler rheinc $1 | python2 src/vm/vm.py

