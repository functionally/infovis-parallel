#!/bin/bash

kafka-device-spacenav laptop/spacenav.yaml &

../dist/build/infovis-parallel/infovis-parallel solo ENB2012.yaml presentation.yaml demo/world.yaml demo/input.yaml &

wait
