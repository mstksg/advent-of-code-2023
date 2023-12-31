{
  description = "aoc2023";
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            aoc2023 = final.haskell-nix.project' {
              name = "aoc2023";
              src = ./.;
              compiler-nix-name = "ghc948";
              shell = {
                withHoogle = false;
                tools = {
                  cabal = { };
                  # stack = { };
                  hlint = { };
                  haskell-language-server = { };
                };
              };
            };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.aoc2023.flake { };
      in
      flake // { packages.default = flake.packages."aoc2023:exe:aoc2023"; }
    );
}
