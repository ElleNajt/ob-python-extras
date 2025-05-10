{ pkgs ? import <nixpkgs> {
  overlays = [

    (final: prev: {

      python3 = prev.python3.override {
        packageOverrides = pfinal: pprev: {
          dataframe_image = final.python3Packages.callPackage
            /home/elle/code/nixpkgs/pkgs/development/python-modules/dataframe_image/default.nix {
              doCheck = false;
            };
        };
      };
    }

    )

  ];

} }:

let
  pythonEnv = pkgs.python3.withPackages (ps:
    with ps; [
      pandas
      numpy
      scikit-learn
      matplotlib
      seaborn
      polars
      ipdb
      rich
      pyarrow
      distutils
      pyspark
      # reladiff
      # dataframe_image
      # jinja2
      plotly
      # python3Packages.better-exceptions # or better-exceptions
      ipython
    ]);
in pkgs.mkShell { buildInputs = [ pythonEnv pkgs.pyright ]; }
