#!/bin/bash

REMOTE=$2
CFG=$1
EXE=dist/build/infovis-parallel/infovis-parallel

echo Configutation: $CFG

for p in `sed -n -e '/port/ {s/^.*port.*['"'"'"]\(.*\)['"'"'"].*$/\1/ ; p}' $CFG`
do
  echo Creating reverse tunnel to host $REMOTE on port $p . . .
  ssh -f -N -R $p:localhost:$p $REMOTE &
  sleep 1s
done

wait
