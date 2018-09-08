{
  mkDerivation, stdenv
, base, ghc-prim
, vulkan
}:
mkDerivation {
  pname = "vulkan-api";
  version = "1.1.3.0";
  libraryHaskellDepends = [
    base
    ghc-prim
  ];
  librarySystemDepends = [
    vulkan
  ];
  homepage = "https://github.com/achirkin/genvulkan#readme";
  description = "Low-level low-overhead vulkan api bindings";
  license = stdenv.lib.licenses.bsd3;
  hydraPlatforms = stdenv.lib.platforms.none;
}
