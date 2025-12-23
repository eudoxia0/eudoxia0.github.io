{
  description = "Jekyll development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
        {
          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              jekyll
              pandoc
              sass
            ];

            shellHook = ''
              export GEM_HOME="$PWD/.gem"
              export PATH="$GEM_HOME/bin:$PATH"
            '';
          };
        });
}
