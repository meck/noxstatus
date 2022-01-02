self: super: {
  haskellPackages = with self.haskell.lib;
    super.haskellPackages.extend (hself: hsuper: { });
}
