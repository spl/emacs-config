#!/bin/sh -e

# Symbolic link with force and don't follow existing link
LN="ln -nsf --"

# Full directory path of script
SRC=$(cd -P -- "$(dirname -- "$0")" && pwd -P)

$LN "$SRC/.emacs" "$HOME/"
