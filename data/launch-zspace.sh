#!/bin/bash

CFG="$* zspace/world.yaml zspace/input.yaml"
EXE=../dist/build/infovis-parallel/infovis-parallel

echo Configutation: $CFG

$EXE +RTS -N6 -RTS solo $CFG
