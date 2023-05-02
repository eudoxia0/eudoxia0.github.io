{ pkgs ? import <nixpkgs> { } }:

let
  my-python = pkgs.python310;
  python-with-my-packages = my-python.withPackages
    (p: with p; [ matplotlib ]);
in pkgs.mkShell {
  buildInputs = [
    python-with-my-packages
  ];
  shellHook = ''
    PYTHONPATH=${python-with-my-packages}/${python-with-my-packages.sitePackages}
  '';
}
