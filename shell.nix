{ pkgs ? import <nixpkgs> { } }:

let
  base-python = pkgs.python311;
  python = base-python.withPackages
    (p: with p; [ matplotlib tkinter ]);
in pkgs.mkShell {
  buildInputs = [
    python
    pkgs.tcl
    pkgs.tk
    pkgs.ruff
    pkgs.graphviz
    pkgs.sass
    pkgs.pandoc
    pkgs.ruff
    pkgs.black
    pkgs.isort
    pkgs.gnumake
  ];
  shellHook = ''
    export TK_LIBRARY="${pkgs.tk}/lib/${pkgs.tk.libPrefix}"
    PYTHONPATH=${python-with-my-packages}/${python-with-my-packages.sitePackages}
  '';
}
