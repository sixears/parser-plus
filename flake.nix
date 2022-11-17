{
  description = "manage info.yaml";

  inputs = {
    nixpkgs.url      = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url  = "github:sixears/flake-build-utils/r1.0.0.11";

    more-unicode.url         = "github:sixears/more-unicode/r0.0.17.8";
    natural.url              = "github:sixears/natural/r0.0.1.9";
    non-empty-containers.url = "github:sixears/non-empty-containers/r1.4.3.14";
    tasty-plus.url           = "github:sixears/tasty-plus/r1.5.2.12";
  };

  outputs = { self, nixpkgs, build-utils
            , more-unicode, natural, non-empty-containers, tasty-plus }:
    build-utils.lib.hOutputs self nixpkgs "parser-plus" {
      deps = {
        inherit more-unicode natural non-empty-containers tasty-plus;
      };
      ghc = p: p.ghc8107; # for tfmt
    };
}
