{ usePinned ? false }:
let

  pkgs = if usePinned then import ./nixpkgs.nix else import <nixpkgs> { };

in
pkgs.callPackage ./derivation.nix {
  inherit pkgs;
}
