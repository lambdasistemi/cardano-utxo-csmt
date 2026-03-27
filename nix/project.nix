{ CHaP, indexState, pkgs, mkdocs, asciinema, cardano-cli, cardano-node-pkgs, ...
}:

let
  indexTool = { index-state = indexState; };
  fix-libs = { lib, pkgs, ... }: {
    packages.cardano-crypto-praos.components.library.pkgconfig =
      lib.mkForce [ [ pkgs.libsodium-vrf ] ];
    packages.cardano-crypto-class.components.library.pkgconfig =
      lib.mkForce [[ pkgs.libsodium-vrf pkgs.secp256k1 pkgs.libblst ]];
  };
  shell = { pkgs, ... }: {
    tools = {
      cabal = indexTool;
      cabal-fmt = indexTool;
      haskell-language-server = indexTool;
      hoogle = indexTool;
      fourmolu = indexTool;
      hlint = indexTool;
      implicit-hie = indexTool;
    };
    withHoogle = true;
    buildInputs = [
      pkgs.just
      pkgs.nixfmt-classic
      pkgs.shellcheck
      pkgs.zstd
      pkgs.mkdocs
      pkgs.marp-cli
      mkdocs.from-nixpkgs
      mkdocs.asciinema-plugin
      mkdocs.markdown-callouts
      asciinema.compress
      asciinema.resize
      pkgs.asciinema
      cardano-cli."cardano-cli:exe:cardano-cli"
      cardano-node-pkgs.cardano-node
    ];
    shellHook = ''
      echo "Entering shell for cardano-utxo CLI development"
    '';
  };

  mkProject = ctx@{ lib, pkgs, ... }: {
    name = "cardano-utxo";
    src = ./..;
    compiler-nix-name = "ghc984";
    shell = shell { inherit pkgs; };
    modules = [
      fix-libs
      ({ lib, ... }: { packages.cardano-utxo-csmt.flags.werror = true; })
    ];
    inputMap = { "https://chap.intersectmbo.org/" = CHaP; };
  };

  project = pkgs.haskell-nix.cabalProject' mkProject;

in {
  devShells.default = project.shell;
  inherit project;
  packages.bench = project.hsPkgs.cardano-utxo-csmt.components.benchmarks.bench;
  packages.unit-tests =
    project.hsPkgs.cardano-utxo-csmt.components.tests.unit-tests;
  packages.database-tests =
    project.hsPkgs.cardano-utxo-csmt.components.tests.database-tests;
  packages.cardano-utxo =
    project.hsPkgs.cardano-utxo-csmt.components.exes.cardano-utxo;
  packages.cardano-utxo-swagger =
    project.hsPkgs.cardano-utxo-csmt.components.exes.cardano-utxo-swagger;
  packages.integration-tests =
    project.hsPkgs.cardano-utxo-csmt.components.tests.cardano-utxo-csmt-integration-test;
  packages.e2e-tests =
    project.hsPkgs.cardano-utxo-csmt.components.tests.e2e-tests;
  packages.cardano-utxo-profiled =
    project.hsPkgs.cardano-utxo-csmt.components.exes.cardano-utxo.profiled;
  packages.bench-profiled =
    project.hsPkgs.cardano-utxo-csmt.components.benchmarks.bench.profiled;
}
