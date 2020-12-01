{ pkgs ? import <nixos> { }, ghc ? "ghc865" }:
let
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

  # TODO: don't build haddocks for dependencies:
  haskellPackages = pkgs.haskell.packages."${ghc}".override {
    overrides = new: old: {
      xmobar =
        old.xmobar.overrideAttrs (old: { configureFlags = "-f with_xft"; });
      config-xmonad = new.callCabal2nixWithOptions "config-xmonad" src
        "--no-haddock --no-check" { };
    };
  };

  shell = let
    ghcide-nix = import ./nix/ghcide-nix { inherit pkgs; };
    ormolu = import ./nix/ormolu { inherit pkgs; };
  in haskellPackages.shellFor {
    packages = hs: [ hs.config-xmonad ];
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

in if pkgs.lib.inNixShell then shell else haskellPackages.config-xmonad
