{ pkgs ? import <nixpkgs> { } }:

let
  pythonEnv = pkgs.python3.withPackages (ps:
    with ps; [
      pandas
      numpy
      scikit-learn
      matplotlib
      seaborn
      rich
      ipython
    ]);
in pkgs.mkShell { buildInputs = [ pythonEnv pkgs.pyright ]; }
