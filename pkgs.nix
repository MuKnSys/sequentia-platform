let
  systemPkgs = import <nixpkgs> {};
  nixpkgs-source = systemPkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "0deaf4d5d224fac3cb2ae9c92a4e349c277be982";
    sha256 = "sha256-uERpVxRrCUB7ySkGb3NtDmzEkPDn23VfkCtT2hJZty8=";
  };
  sequentia-source = systempkgs.fetchFromGitHub {
    owner = "MuKnIO";
    repo = "SEQ-Core-Elements";
    rev = "ac958ffe6b0963059ca2b21df5d9e7150474b07a";
    sha256 = "sha256-G0EA/FYrPWHjLGKWW3+OVzCoThtfqoFt/9gFgaOI8Aw="; 
  };
  config = {
    packageOverrides = pkgs: rec {
      sequentia = pkgs.callPackage sequentia-source {};
    };
  };
  pkgs = import nixpkgs-source { inherit config; };
in pkgs
