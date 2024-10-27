{ pkgs ? import <nixpkgs> { } }:

let
  pythonEnv = pkgs.python3.withPackages
    (ps: with ps; [ pandas numpy scikit-learn matplotlib seaborn ]);
in pkgs.mkShell { buildInputs = [ pythonEnv pkgs.pyright ]; }
