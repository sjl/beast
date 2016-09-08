#!/usr/bin/env bash

set -e

which figlet >/dev/null && FIG="figlet -kf $1" || FIG="echo"
which lolcat >/dev/null && LOL="lolcat --freq=0.25" || LOL="cat"

shift

echo

$FIG "$@" | sed -Ee 's/ +$$//' | tr -s '\n' | $LOL

echo
