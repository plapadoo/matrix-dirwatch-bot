let
  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs-channels";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src { };
  thisPackage = pkgs.haskellPackages.callCabal2nix "matrix-dirwatch" ./. {};
in
  pkgs.dockerTools.buildImage {
    name = "matrix-dirwatch-nohttp-bot";
    tag = "latest";
    contents = pkgs.haskell.lib.justStaticExecutables thisPackage;
  }
