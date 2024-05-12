let
  systemPkgs = import <nixpkgs> {};
  nixpkgs-source = systemPkgs.fetchFromGitHub {
    owner = "MuKnIO";
    repo = "nixpkgs";
    rev = "8b9c37af42da9e1c3a48d3befbc87bc83ff939a6";
    sha256 = "sha256-WfiWW430xmb6mhaaDqB93e7gGuIXMV4oWqRXALeNvi8=";
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
