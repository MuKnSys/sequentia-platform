let
  systemPkgs = import <nixpkgs> {};
  nixpkgs-source = systemPkgs.fetchFromGitHub {
    owner = "MuKnIO";
    repo = "nixpkgs";
    rev = "7e0ac2b84a76241917f8458d4050fd3316f9588c";
    sha256 = "sha256-T5TCY6wJkmez4LZJAjR6TVOlT5XPryTD18ydUJ9x9rU=";
  };
  sequentia-source = systemPkgs.fetchFromGitHub {
    owner = "MuKnIO";
    repo = "SEQ-Core-Elements";
    rev = "6eb6a3262408a9d2d9f61b27dbc33180705032b9";
    sha256 = "0gb9byglaxbgl49m6fl073wv8jl24rgfdwh1p604lcyigzryf9gp";
  };
  config = {
    packageOverrides = pkgs: rec {
      sequentia = pkgs.callPackage sequentia-source {};
    };
  };
  pkgs = import nixpkgs-source { inherit config; };
in pkgs
