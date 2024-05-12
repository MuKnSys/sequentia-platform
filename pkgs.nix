let
  systemPkgs = import <nixpkgs> {};
  nixpkgs-source = systemPkgs.fetchFromGitHub {
    owner = "MuKnIO";
    repo = "nixpkgs";
    rev = "4237c846ca9a370abc5de19e0de30813134c2ba4";
    sha256 = "sha256-T5TCY6wJkmez4LZJAjR6TVOlT5XPryTD18ydUJ9x9rU=";
  };
  sequentia-source = systemPkgs.fetchFromGitHub {
    owner = "MuKnIO";
    repo = "SEQ-Core-Elements";
    rev = "76a651841a04b7b4a383d646b8c13a89585cce96";
    sha256 = "sha256-A0QNC55ih1pBh2RXGreTvUSNYS1iHVc5IL00IHr4tkQ=";
  };
  config = {
    packageOverrides = pkgs: rec {
      #sequentia = pkgs.callPackage sequentia-source {};
      sequentiaPkgs = "${sequentia-source}/pkgs.nix";
      sequentia = (import sequentiaPkgs).sequentia;
    };
  };
  pkgs = import nixpkgs-source { inherit config; };
in pkgs
