{ ghc ? null # not null if called by `stack build`
, pkgs ? import ./nixpkgs { } }:
let
  ghcide-nix = import (pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "ghcide-nix";
    rev = "fd42f62613d491565c1676821148c008b2011584";
    sha256 = "1cd60hlr58ih5j21a64rdrmyrh28dhwz0wgmkb3p3j8jmrya8fv7";
  }) { };

  ormolu = import (pkgs.fetchFromGitHub {
    owner = "tweag";
    repo = "ormolu";
    rev = "3abadaefa5e190ff346f9aeb309465ac890495c2";
    sha256 = "0vqrb12bsp1dczff3i5pajzhjwz035rxg8vznrgj5p6j7mb2vcnd";
  }) { inherit pkgs; };
in if builtins.isNull ghc then
# Development shell
  pkgs.mkShell {
    buildInputs =
      [ pkgs.nodePackages.prettier ghcide-nix.ghcide-ghc865 ormolu.ormolu ];
  }
else
# stack build
  pkgs.haskell.lib.buildStackProject {
    inherit ghc;
    name = "xmonad-env";
    buildInputs = [
      pkgs.pkgconfig
      pkgs.autoconf
      pkgs.gcc
      pkgs.alsaLib
      pkgs.xorg.libX11
      pkgs.xorg.libXext
      pkgs.xorg.libXft
      pkgs.xorg.libXinerama
      pkgs.xorg.libXpm
      pkgs.xorg.libXrandr
      pkgs.xorg.libXrender
      pkgs.xorg.libXScrnSaver
    ];
  }
