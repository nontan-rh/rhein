#!/usr/bin/env sh

gosh -I src/compiler rheinc $1 | gosh -I src/assembler vmasm | rheinvm

