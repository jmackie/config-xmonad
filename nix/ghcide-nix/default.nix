{ pkgs }:
import (pkgs.fetchFromGitHub {
  owner = "hercules-ci";
  repo = "ghcide-nix";
  rev = "fd42f62613d491565c1676821148c008b2011584";
  sha256 = "1cd60hlr58ih5j21a64rdrmyrh28dhwz0wgmkb3p3j8jmrya8fv7";
}) { }
