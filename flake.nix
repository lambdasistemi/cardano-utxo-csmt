{
  description =
    "CSMT UTxO, Cardano UTxO tracking via compact sparse merkle trees";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    mkdocs.url = "github:paolino/dev-assets?dir=mkdocs";
    asciinema.url = "github:paolino/dev-assets?dir=asciinema";
    cardano-cli.url = "github:IntersectMBO/cardano-cli";
    cardano-node.url = "github:IntersectMBO/cardano-node/10.5.4";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, haskellNix, mkdocs, asciinema
    , cardano-cli, cardano-node, iohkNix, CHaP, ... }:
    let
      version = self.dirtyShortRev or self.shortRev;
      parts = flake-parts.lib.mkFlake { inherit inputs; } {
        systems = [ "x86_64-linux" "aarch64-darwin" ];
        perSystem = { system, ... }:
          let
            pkgs = import nixpkgs {
              overlays = [
                iohkNix.overlays.crypto
                haskellNix.overlay
                iohkNix.overlays.haskell-nix-crypto
                iohkNix.overlays.cardano-lib
              ];
              inherit system;
            };
            project = import ./nix/project.nix {
              indexState = "2025-12-07T00:00:00Z";
              inherit CHaP;
              inherit pkgs;
              cardano-cli = cardano-cli.packages.${system};
              cardano-node-pkgs = cardano-node.packages.${system};
              mkdocs = mkdocs.packages.${system};
              asciinema = asciinema.packages.${system};
            };

            docker-image = pkgs.callPackage ./nix/docker-image.nix {
              inherit version;
              inherit project;
            };

            docker-image-profiled = pkgs.dockerTools.buildImage {
              name = "ghcr.io/paolino/cardano-utxo-csmt/cardano-utxo-csmt";
              tag = "${version}-profiled";
              config = { EntryPoint = [ "cardano-utxo" ]; };
              copyToRoot = pkgs.buildEnv {
                name = "image-root";
                paths = [ project.packages.cardano-utxo-profiled ];
              };
            };

          in rec {
            packages = {
              inherit (project.packages)
                bench unit-tests database-tests integration-tests e2e-tests
                cardano-utxo cardano-utxo-swagger cardano-utxo-profiled
                bench-profiled;
              inherit docker-image docker-image-profiled;
              default = packages.cardano-utxo;
            };
            inherit (project) devShells;
          };
      };
    in {
      inherit (parts) packages devShells;
      inherit version;
    };

}
