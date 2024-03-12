{ withLocalSequentia ? false 
}:
let pkgs = import ./pkgs.nix;
    sequentia = if withLocalSequentia then pkgs.callPackage ../SEQ-Core-Elements/default.nix {} else pkgs.sequentia;
    gerbilPackages = [ 
        pkgs.gerbilPackages-unstable.gerbil-utils
        pkgs.gerbilPackages-unstable.gerbil-poo
    ]; 
in pkgs.mkShell {
    buildInputs = gerbilPackages ++ [
        pkgs.gerbil-unstable
        sequentia
    ];

    shellHook = ''
        export GERBIL_LOADPATH=${pkgs.gerbil-support.gerbilLoadPath (["$out"] ++ gerbilPackages)}
        export GERBIL_PATH=$PWD/.build
        export GERBIL_BUILD_CORES=$NIX_BUILD_CORES
    '';
} 
