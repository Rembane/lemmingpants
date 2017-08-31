{ mkDerivation, aeson, base, bytestring, cereal, cereal-text
, cereal-vector, containers, directory, errors, exceptions
, http-media, megaparsec, mtl, servant, servant-server, stdenv, stm
, text, time, uuid, vector, wai, wai-logger, wai-websockets, warp
, websockets
}:
mkDerivation {
  pname = "lemmingpants";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring cereal cereal-text cereal-vector containers
    directory errors exceptions http-media mtl servant servant-server
    stm text time uuid vector wai-websockets websockets
  ];
  executableHaskellDepends = [
    base megaparsec servant servant-server stm text uuid wai wai-logger
    warp
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/Rembane/lemmingpants#readme";
  license = stdenv.lib.licenses.asl20;
}
