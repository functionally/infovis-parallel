{
  nixpkgs  ? import <nixos-unstable>
, compiler ? "ghc822"
}:
let
  pkgs = nixpkgs { };
  infovis-parallel = ((import ./release.nix) {nixpkgs = nixpkgs; compiler = compiler;}).infovis-parallel;
  haskellPackages = pkgs.haskell.packages."${compiler}";
in
  pkgs.lib.overrideDerivation infovis-parallel.env (old: {
    buildInputs = old.buildInputs ++ [
      haskellPackages.cabal-install
    # haskellPackages.ghc-mod
      haskellPackages.ghcid
    # haskellPackages.hasktags
      haskellPackages.hdevtools
      haskellPackages.hindent
      haskellPackages.hlint
      haskellPackages.pointfree
      haskellPackages.pointful
      pkgs.glslang
      pkgs.vulkan-loader
    ];
})
