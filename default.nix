{ mkDerivation, aeson, base, bytestring, errors, exceptions
, http-media, mtl, selda, selda-sqlite, servant, servant-server
, stdenv, text, time, wai, wai-logger, warp
}:
mkDerivation {
  pname = "lemmingpants";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring errors exceptions http-media mtl selda
    selda-sqlite servant servant-server text time
  ];
  executableHaskellDepends = [
    base selda-sqlite servant servant-server wai wai-logger warp
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/Rembane/lemmingpants#readme";
  license = stdenv.lib.licenses.asl20;
}
