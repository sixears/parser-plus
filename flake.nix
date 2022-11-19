{
  description = "manage info.yaml";

  inputs = {
    nixpkgs.url      = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url  = github:sixears/flake-build-utils/r1.0.0.13;

    more-unicode.url         = github:sixears/more-unicode/r0.0.17.11;
    natural.url              = github:sixears/natural/r0.0.1.13;
    non-empty-containers.url = github:sixears/non-empty-containers/r1.4.3.34;
    tasty-plus.url           = github:sixears/tasty-plus/r1.5.2.22;
  };

  outputs = { self, nixpkgs, build-utils
            , more-unicode, natural, non-empty-containers, tasty-plus }:
    build-utils.lib.hOutputs self nixpkgs "parser-plus" {
      ghc = p: p.ghc8107; # for tfmt
      callPackage = { mkDerivation, lib, mapPkg, system
                    , base, base-unicode-symbols, data-textual, mono-traversable
                    , mtl, nonempty-containers, parsec, parsers, tasty
                    , tasty-hunit
                    }:
        mkDerivation {
          pname = "parser-plus";
          version = "1.0.7.26";
          src = ./.;
          libraryHaskellDepends = [
            base base-unicode-symbols data-textual mono-traversable mtl
            nonempty-containers parsec parsers tasty tasty-hunit
          ] ++ mapPkg [ more-unicode natural non-empty-containers tasty-plus ];
          testHaskellDepends = [ base tasty ];
          description = "manage info.yaml";
          license = lib.licenses.mit;
        };
    };
}
