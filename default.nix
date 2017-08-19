{ mkDerivation, aeson, base, bytestring, errors, exceptions
, http-media, mtl, selda, selda-sqlite, servant, servant-server
, stdenv, stm, text, time, wai, wai-logger, wai-websockets, warp
, websockets
}:
mkDerivation {
  pname = "lemmingpants";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring errors exceptions http-media mtl selda
    selda-sqlite servant servant-server stm text time wai-websockets
    websockets
  ];
  executableHaskellDepends = [
    base selda-sqlite servant servant-server stm wai wai-logger warp
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/Rembane/lemmingpants#readme";
  license = stdenv.lib.licenses.asl20;
}
