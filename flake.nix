
{
  description = "Profunctor Optics";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-23.05;

    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      overlay = import ./overlay.nix;
      overlays = [ overlay ];
    in flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let pkgs = import nixpkgs { inherit system overlays; };
      in rec {
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ p.profunctor-optics ];
          buildInputs = [
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.ghc
            pkgs.haskellPackages.haskell-language-server
            pkgs.nixfmt
          ];
        };
        packages.default = pkgs.haskellPackages.profunctor-optics;
        packages.co-prompt = pkgs.haskellPackages.profunctor-optics;
      }) // {
        overlays.default = overlay;
      };
}
