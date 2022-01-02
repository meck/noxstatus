{
  description = "A Hello World in Haskell with a dependency and a devShell";
  inputs.nixpkgs.url = "nixpkgs";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = final: prev: {
        noxstatus = final.haskellPackages.callCabal2nix "noxstatus" ./. { };
      };
      packages = forAllSystems (system: {
        inherit (nixpkgsFor.${system}) noxstatus;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.noxstatus);
      checks = self.packages;
      devShell = forAllSystems (system:
        let inherit (nixpkgsFor.${system}) haskellPackages;
        in
        haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.noxstatus ];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            cabal-install
          ];
        });
    };
}
