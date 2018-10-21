#!/usr/bin/env nix-shell
#!nix-shell -i bash -p "haskellPackages.ghcWithPackages (pkgs: [pkgs.base64-bytestring pkgs.text pkgs.websockets])"
