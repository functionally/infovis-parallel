{
  mkDerivation, stdenv
, base, bindings-DSL, HUnit, libGL, libX11, libXcursor, libXext, libXfixes, libXi, libXinerama, libXrandr, libXxf86vm
}:
mkDerivation {
  pname = "bindings-GLFW";
  version = "3.2.1.1";
  sha256 = "03hn12bdqypwd6x9hxa7685bm6w86nsf6cmcwkr8zzgb41mwc93b";
  libraryHaskellDepends = [
    base
    bindings-DSL
  ];
  librarySystemDepends = [
    libGL
    libX11
    libXcursor
    libXext
    libXfixes
    libXi
    libXinerama
    libXrandr
    libXxf86vm
  ];
  description = "Low-level bindings to GLFW OpenGL library";
  license = stdenv.lib.licenses.bsd3;
  doCheck = false;
}
