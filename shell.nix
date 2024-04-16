{ withLocalSequentia ? true
}:
let pkgs = import ./pkgs.nix;
    sequentia = if withLocalSequentia then import ../SEQ-Core-Elements/default.nix else pkgs.sequentia;
    gerbilPackages = [
        pkgs.gerbilPackages-unstable.gerbil-crypto
        pkgs.gerbilPackages-unstable.gerbil-utils
        pkgs.gerbilPackages-unstable.gerbil-poo
    ];
in pkgs.mkShell {
    inputsFrom = pkgs.lib.optionals withLocalSequentia [sequentia];

    buildInputs = gerbilPackages ++ [
        pkgs.gerbil-unstable
        pkgs.asciinema
        pkgs.asciinema-agg
        pkgs.jq
        pkgs.clang-tools
    ] ++ pkgs.lib.optionals (!withLocalSequentia) [sequentia];

    shellHook = ''
        export GERBIL_LOADPATH=${pkgs.gerbil-support.gerbilLoadPath (["$out"] ++ gerbilPackages)}
        export GERBIL_PATH=$PWD/.build
        export GERBIL_BUILD_CORES=$NIX_BUILD_CORES
        ${if withLocalSequentia then
           let sce = ../SEQ-Core-Elements/src ; in
           "PATH=${sce}:$PATH" else ""}
    '';
}
