#!/bin/bash

DAT=data/ENB2012.tsv
CFG=data/ESIF.json
EXE=dist/build/infovis-parallel/infovis-parallel

for p in `sed -n -e '/port/ {s/^.*"port".*"\(.*\)".*$/\1/ ; p}' $CFG`
do
  echo Starting slave on port $p . . .
  $EXE planes-cave --port=$p &
  sleep 1s
done

sleep 5s

echo Starting master on port 44444 . . .
$EXE planes-cave --displays=$CFG $DAT

wait
