{ mkDerivation, stdenv
, aeson, base, binary, containers, data-default, deepseq, GLFW-b, linear, vector, vulkan-api, yaml
}:
mkDerivation {
  pname = "infovis-parallel";
  version = "0.4.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary containers data-default deepseq GLFW-b linear vector vulkan-api yaml
  ];
  executableHaskellDepends = [
    aeson base binary containers data-default deepseq GLFW-b linear vector vulkan-api yaml
  ];
  homepage = "https://bitbucket.org/bwbush/infovis-parallel";
  description = "Parallel-planes information visualization";
  license = stdenv.lib.licenses.mit;
}
