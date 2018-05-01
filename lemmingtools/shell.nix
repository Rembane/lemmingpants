{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, megaparsec
      , postgresql-simple, stdenv, text
      }:
      mkDerivation {
        pname = "lemmingtools";
        version = "0.0.1";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base bytestring megaparsec postgresql-simple text
        ];
        executableHaskellDepends = [
          base bytestring megaparsec postgresql-simple text
        ];
        testHaskellDepends = [ base ];
        homepage = "https://github.com/githubuser/lemmingtools#readme";
        description = "Tools for helping out with Lemmingpants stuff";
        license = stdenv.lib.licenses.asl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
