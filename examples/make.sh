#!/usr/bin/env nix-shell
#!nix-shell -i bash -p protobuf

for f in *.pbt
do
  protoc -I ../standalone --encode=Infovis.Request infovis.proto3 < $f > ${f%.pbt}.pbb
done
