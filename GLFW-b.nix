{
  mkDerivation, stdenv
, base, bindings-GLFW, deepseq
}:
mkDerivation {
  pname = "GLFW-b";
  version = "3.2.1.0";
  sha256 = "19mngkprzlm322pfyljvm4lyk1j7j8ss50m5kzzmkwk3mph25h1i";
  libraryHaskellDepends = [
    base
    bindings-GLFW
    deepseq
  ];
  description = "Bindings to GLFW OpenGL library";
  license = stdenv.lib.licenses.bsd3;
  doCheck = false;
}
