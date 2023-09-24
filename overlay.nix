final: prev: {
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
    (hfinal: hprev: {
      profunctor-optics = hfinal.callCabal2nix "profunctor-optics" (./.) { };
      });
  });
}
