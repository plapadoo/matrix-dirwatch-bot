let
  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs-channels";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src { };

  drv = pkgs.haskellPackages.callCabal2nix "matrix-dirwatch" ./. {};
in
  if pkgs.lib.inNixShell then drv.env else pkgs.haskell.lib.justStaticExecutables drv
