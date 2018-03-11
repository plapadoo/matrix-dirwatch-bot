{ pkgs ? import <nixpkgs> {} }:
let
  drv = pkgs.haskellPackages.callCabal2nix "matrix-dirwatch" ./. {};
in
  if pkgs.lib.inNixShell then drv.env else drv
