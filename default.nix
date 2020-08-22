{ pkgs ? import <nixpkgs> { }, compiler ? "default", usePinnedNixpkgs ? false
, clientSecret }:
let

  # Known working nixpkgs:
  pinnedPkgs = import (builtins.fetchTarball {
    name = "nixos-unstable-2020-08-22";
    url =
      "https://github.com/nixos/nixpkgs/archive/c59ea8b8a0e7f927e7291c14ea6cd1bd3a16ff38.tar.gz";
    sha256 = "1ak7jqx94fjhc68xh1lh35kh3w3ndbadprrb762qgvcfb8351x8v";
  }) { };

  p = if usePinnedNixpkgs then pinnedPkgs else pkgs;

  myHaskellPackages = if compiler == "default" then
    p.haskellPackages
  else
    p.haskell.packages.${compiler};

  drv = myHaskellPackages.callCabal2nix "noxstatus" ./. { };

in p.haskell.lib.overrideCabal drv (old: {
  postConfigure = ''
    substituteInPlace app/ClientSecret.hs --replace '"CLIENTSECRET GOES HERE"' '"${clientSecret}"'
  '';
})
