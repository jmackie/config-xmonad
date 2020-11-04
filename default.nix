{ pkgs ? import <nixos> { }, ghc ? "ghc865" }:
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

  # TODO: use haskellPackages overrides

  shell = let
    ghcide-nix = import ./nix/ghcide-nix { inherit pkgs; };
    ormolu = import ./nix/ormolu { inherit pkgs; };
  in haskellPackages.shellFor {
    packages = p: [ drv ];
    buildInputs = [
      # developing
      pkgs.cabal-install
      pkgs.ghcid

      # linters
      pkgs.hlint
      pkgs.shellcheck

      # formatters
      pkgs.ormolu
      pkgs.shfmt
      pkgs.nodePackages.prettier
    ];
    withHoogle = false;
  };

in if pkgs.lib.inNixShell then shell else drv
