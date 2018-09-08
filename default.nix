{ mkDerivation, stdenv
, aeson, base, binary, data-default, deepseq, linear, vector, yaml
, containers, directory, filepath, GLFW-b, process, template-haskell, vulkan-api
}:
mkDerivation {
  pname = "infovis-parallel";
  version = "0.4.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary data-default deepseq linear vector yaml
  ];
  executableHaskellDepends = [
    aeson base binary containers data-default deepseq directory filepath GLFW-b linear process template-haskell vector vulkan-api yaml
  ];
  homepage = "https://bitbucket.org/bwbush/infovis-parallel";
  description = "Parallel-planes information visualization";
  license = stdenv.lib.licenses.mit;
}
