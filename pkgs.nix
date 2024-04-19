let
  systemPkgs = import <nixpkgs> {};
  nixpkgs-source = systemPkgs.fetchFromGitHub {
    owner = "MuKnIO";
    repo = "nixpkgs";
    rev = "98803955f43860497cdb785343448d6c454aaede";
    sha256 = "sha256-+UoKXNKJPxXc6Tb8znIKC+wptAhwg6bqP3RPQqICNC0=";
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
