let
  systemPkgs = import <nixpkgs> {};
  nixpkgs-source = systemPkgs.fetchFromGitHub {
    owner = "MuKnIO";
    repo = "nixpkgs";
    rev = "7e0ac2b84a76241917f8458d4050fd3316f9588c";
    sha256 = "sha256-9DsfjcWPrCV6B1rMMtAczg/WtyOf2lsw+ob8otfqmOU=";
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
