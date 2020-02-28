{ pkgs ? import ./nix/nixpkgs { }, ghc ? "ghc865" }:
let
  haskellPackages = pkgs.haskell.packages."${ghc}";
  drv = haskellPackages.callCabal2nix "config-xmonad" ./. { };
  shell = let
    ghcide-nix = import ./nix/ghcide-nix { inherit pkgs; };
    ormolu = import ./nix/ormolu { inherit pkgs; };
  in haskellPackages.shellFor {
    packages = p: [ drv ];
    buildInputs = [
      ghcide-nix."ghcide-${ghc}"
      pkgs.ghcid
      pkgs.hlint
      ormolu.ormolu
      pkgs.nodePackages.prettier
      pkgs.shfmt
      pkgs.shellcheck
    ];
    withHoogle = false;
  };

in if pkgs.lib.inNixShell then shell else drv
