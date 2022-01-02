let
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-unstable-2020-10-25";
    url =
      "https://github.com/nixos/nixpkgs/archive/00941cd747e9bc1c3326d1362dbc7e9cfe18cf53.tar.gz";
    sha256 = "12mjfar2ir561jxa1xvw6b1whbqs1rq59byc87icql399zal5z4a";
  };
in (import nixpkgs) { overlays = [ (import ./overlay.nix) ]; }
