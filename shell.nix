{ withLocalSequentia ? true
}:
# Override the default with:
#  nix-shell --arg withLocalSequentia false

let pkgs = import ./pkgs.nix;
    sequentia = if withLocalSequentia then
       import ../SEQ-Core-Elements/default.nix else
       pkgs.sequentia;
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

    # Until gerbil-support includes static compilation objects,
    # let's not use gerbilLoadPath and instead copy the sources into the writable GERBIL_PATH.
    shellHook = ''
        export GERBIL_PATH=$PWD/.gerbil
        mkdir -p $GERBIL_PATH
        chmod -R u+w .gerbil
        #export GERBIL_LOADPATH=${pkgs.gerbil-support.gerbilLoadPath (["$out"] ++ gerbilPackages)}
        for i in ${pkgs.lib.concatStringsSep " " gerbilPackages} ; do
          cp -af $i/gerbil/lib $GERBIL_PATH/
          chmod -R u+w .gerbil
        done
        export GERBIL_BUILD_CORES=$NIX_BUILD_CORES
        ${if withLocalSequentia then
           let sce = ../SEQ-Core-Elements/src ; in
           "PATH=${sce}:$PATH" else
           "PATH=${sequentia}/bin:$PATH"}
    '';
}
