{ pkgs ? import ./nix/nixpkgs { }, ghc ? "ghc865" }:
let
  haskellPackages = pkgs.haskell.packages."${ghc}";

  # Be quite strict about filtering files to minimize rebuilding.
  # Otherwise this we recompiles on every startup, which is slow
  # and can also hang if the network is unavailable!
  srcFilter = f: type:
    pkgs.lib.hasPrefix (toString ./src) (toString f)
    || pkgs.lib.elem (toString f)
    (map toString [ ./config-xmonad.cabal ./xmobar.hs ./xmonad.hs ]);

  src = pkgs.lib.cleanSourceWith {
    filter = srcFilter;
    src = ./.;
    name = "config-xmonad";
  };
  drv = haskellPackages.callCabal2nixWithOptions "config-xmonad" src
    "--no-haddock --no-check" { };
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
