let pkgs = import ./pkgs.nix;
    gerbilInputs = [
        pkgs.gerbilPackages-unstable.gerbil-utils
        pkgs.gerbilPackages-unstable.gerbil-poo 
    ];
in pkgs.mkShell {
    buildInputs = [
		pkgs.gerbil-unstable
		pkgs.bitcoin
        pkgs.gdb
    ] ++ gerbilInputs;

    shellHook = ''
        export GERBIL_LOADPATH=${pkgs.gerbil-support.gerbilLoadPath (["$out"] ++ gerbilInputs)}
        export GERBIL_PATH=$PWD/.build
        export GERBIL_BUILD_CORES=$NIX_BUILD_CORES
    '';
} 
