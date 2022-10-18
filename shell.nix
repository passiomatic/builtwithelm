{
  pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/88eab1e431cabd0ed621428d8b40d425a07af39f.tar.gz") {}
}:

pkgs.mkShell {
  buildInputs = [
    pkgs.caddy
    pkgs.elmPackages.elm
    pkgs.nodePackages.terser
  ];

  shellHook =
    ''
    export project="$PWD"
    export build="$project/.build"

    export PATH="$project/bin:$PATH"
    '';
}
