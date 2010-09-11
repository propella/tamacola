#!/bin/sh
DIR=$(dirname $0)
$DIR/cola/cola -b $DIR/boot/boot.k $@
