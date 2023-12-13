{
  description = "A developer shell for working on builtwithelm.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          name = "builtwithelm";

          packages = with pkgs.elmPackages; [
            elm
            elm-format
            elm-json
            elm-optimize-level-2
            elm-review
            pkgs.caddy
            pkgs.nodejs_20
            pkgs.nodePackages.terser
            pkgs.shellcheck
          ];

          shellHook =
            ''
            export project="$PWD"
            export build="$project/.build"
            export PATH="$project/bin:$PATH"

            npm install --loglevel silent
            '';
        };
      }
    );
}
