{ pkgs ? import <nixpkgs> {}, displayrUtils }:

pkgs.rPackages.buildRPackage {
  name = "flipChart";
  version = displayrUtils.extractRVersion (builtins.readFile ./DESCRIPTION); 
  src = ./.;
  description = ''
    Wrapper for other chart functions, such that they can be access via a
    single function, CChart. The chief benefit of this is that a user can change chart
    type without having to change arguments (whcih are automatically mapped to
    the appropriate parameters).
  '';
  propagatedBuildInputs = with pkgs.rPackages; [ 
    flipU
    flipTransformations
    verbs
    flipData
    flipFormat
    flipStatistics
    flipTime
    flipStandardCharts
    flipTables
    flipChartBasics
    abind
  ];
}
