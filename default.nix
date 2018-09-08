{ mkDerivation, stdenv
, aeson, base, binary, data-default, deepseq, GLFW-b, linear, logging-effect, mtl, vector, vulkan-api, yaml
, containers, directory, filepath, process, template-haskell
}:
mkDerivation {
  pname = "infovis-parallel";
  version = "1.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary data-default deepseq GLFW-b linear logging-effect mtl vector vulkan-api yaml
  ];
  executableHaskellDepends = [
    aeson base binary containers data-default deepseq directory filepath GLFW-b linear logging-effect mtl process template-haskell vector vulkan-api yaml
  ];
  homepage = "https://bitbucket.org/bwbush/infovis-parallel";
  description = "Parallel-planes information visualization";
  license = stdenv.lib.licenses.mit;
}
