#!/bin/bash

CFG="$* laptop/world.yaml laptop/input.yaml"
EXE=../dist/build/infovis-parallel/infovis-parallel

echo Configutation: $CFG

$EXE +RTS -N6 -RTS solo $CFG
