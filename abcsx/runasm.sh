#!/bin/sh
ASM=${ASM:-$(dirname $0)/abcsx.ss\ -asm}

$ASM $1 && avmshell $1.abc
