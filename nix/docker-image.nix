{ pkgs, project, version, ... }:

pkgs.dockerTools.buildImage {
  name = "ghcr.io/lambdasistemi/cardano-utxo-csmt/cardano-utxo-csmt";
  tag = version;
  config = { EntryPoint = [ "cardano-utxo" ]; };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths =
      [ project.packages.cardano-utxo.package.components.exes.cardano-utxo ];
  };
}
