let
  systemPkgs = import <nixpkgs> {};
  nixpkgs-source = systemPkgs.fetchFromGitHub {
    owner = "MuKnIO";
    repo = "nixpkgs";
    rev = "ed2c5605b165c6d88eab33fc2988bd6583fedca4";
    sha256 = "sha256-NalftrdrbXNDmnBgJZd4Tnd73LuqfAyrZb22bTkfoEM=";
  };
  sequentia-source = systemPkgs.fetchFromGitHub {
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
