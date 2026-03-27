# shellchec  shell=bash

# shellcheck disable=SC2121
set unstable := true

default:
    just --list

format:
    #!/usr/bin/env bash
    find . -type f -name '*.hs' -not -path '*/dist-newstyle/*' -exec fourmolu -i {} +
    cabal-fmt -i cardano-utxo-csmt.cabal
    nixfmt ./*.nix
    nixfmt nix/*.nix

hlint:
    #!/usr/bin/env bash
    find . -type f -name '*.hs' -not -path '*/dist-newstyle/*' -exec hlint {} +

bench:
    #!/usr/bin/env bash
    cabal bench
unit match="" *args='' :
    #!/usr/bin/env bash
    # shellcheck disable=SC2050
    if [[ '{{ match }}' == "" ]]; then
      cabal test unit-tests \
          --test-show-details=direct \
          --test-options="{{args}}"
    else
      cabal test unit-tests \
          --test-show-details=direct \
          --test-option=--match \
          --test-option="{{ match }}" \
          --test-options="{{ args }}"
    fi

database match="" *args='' :
    #!/usr/bin/env bash
    # shellcheck disable=SC2050
    if [[ '{{ match }}' == "" ]]; then
      cabal test database-tests \
          --test-show-details=direct \
          --test-options="{{args}}"
    else
      cabal test database-tests \
          --test-show-details=direct \
          --test-option=--match \
          --test-option="{{ match }}" \
          --test-options="{{ args }}"
    fi

build:
    #!/usr/bin/env bash
    cabal build all --enable-tests --enable-benchmarks

CI:
    #!/usr/bin/env bash
    set -euo pipefail
    just build
    just unit
    cabal-fmt -c cardano-utxo-csmt.cabal
    find . -type f -name '*.hs' -not -path '*/dist-newstyle/*' -exec fourmolu -m check {} +
    nixfmt -c ./*.nix nix/*.nix
    find . -type f -name '*.hs' -not -path '*/dist-newstyle/*' -exec hlint {} +
    just update-swagger
    git diff --exit-code docs/assets/swagger.json
    just update-diagrams
    git diff --exit-code docs/presentation/light-clients.md


build-docker tag='latest':
    #!/usr/bin/env bash
    nix build .#docker-image
    docker load < result
    version=$(nix eval --raw .#version)
    docker image tag ghcr.io/lambdasistemi/cardano-utxo-csmt/cardano-utxo:"$version" \
        "ghcr.io/lambdasistemi/cardano-utxo-csmt/cardano-utxo:{{ tag }}"

start-docker bg="false":
    #!/usr/bin/env bash
    # shellcheck disable=SC2050
    if [[ '{{ bg }}' == "true" ]]; then
        docker compose -f CD/docker-compose.yaml up -d --remove-orphans
    else
        docker compose -f CD/docker-compose.yaml up --remove-orphans
    fi

build-and-start-docker bg="false":
    #!/usr/bin/env bash
    just build-docker
    just start-docker "{{ bg }}"

logs-docker:
    #!/usr/bin/env bash
    docker compose -f CD/docker-compose.yaml logs -ft

stop-docker:
    #!/usr/bin/env bash
    docker compose -f CD/docker-compose.yaml down

push-docker tag='latest':
    #!/usr/bin/env bash
    docker push "ghcr.io/lambdasistemi/cardano-utxo-csmt/cardano-utxo:{{ tag }}"

release version arch:
    #!/usr/bin/env bash
    set -euo pipefail
    just build-docker
    just push-docker
    ./CI/release.sh "{{ version }}" "{{ arch }}"

integration match="":
    #!/usr/bin/env bash
    set -euo pipefail
    cabal test cardano-utxo-csmt-integration-test \
        --test-show-details=direct \
        --test-option=--match \
        --test-option="{{ match }}"

integration-all:
    #!/usr/bin/env bash
    set -euo pipefail
    cabal test cardano-utxo-csmt-integration-test \
        --test-show-details=direct

# Run genesis E2E test (requires cardano-node in PATH)
e2e-genesis:
    #!/usr/bin/env bash
    set -euo pipefail
    cabal test cardano-utxo-csmt:e2e-tests -O0 \
        --test-show-details=direct

# Build docker image for E2E testing
e2e-build:
    #!/usr/bin/env bash
    set -euo pipefail
    nix build .#docker-image
    loaded=$(docker load < result | grep 'Loaded image:' | sed 's/Loaded image: //')
    echo "Loaded: $loaded"
    docker image tag "$loaded" cardano-utxo:e2e

# Run N2C E2E test against cardano-node devnet
e2e:
    #!/usr/bin/env bash
    set -euo pipefail
    just e2e-build
    docker compose -f e2e/docker-compose.yml down -v 2>/dev/null || true
    docker compose -f e2e/docker-compose.yml up \
        --abort-on-container-exit \
        --exit-code-from e2e-test
    docker compose -f e2e/docker-compose.yml down -v

# Run N2N E2E test against cardano-node devnet
e2e-n2n:
    #!/usr/bin/env bash
    set -euo pipefail
    just e2e-build
    docker compose -f e2e/docker-compose.n2n.yml down -v 2>/dev/null || true
    docker compose -f e2e/docker-compose.n2n.yml up \
        --abort-on-container-exit \
        --exit-code-from e2e-test
    docker compose -f e2e/docker-compose.n2n.yml down -v

# Tear down E2E environment
e2e-down:
    #!/usr/bin/env bash
    docker compose -f e2e/docker-compose.yml down -v
    docker compose -f e2e/docker-compose.n2n.yml down -v 2>/dev/null || true

update-swagger:
    #!/usr/bin/env bash
    nix run .#cardano-utxo-swagger > docs/assets/swagger.json

# Serve documentation locally (finds free port)
serve-docs:
    #!/usr/bin/env bash
    set -euo pipefail
    port=8000
    while nc -z localhost $port 2>/dev/null; do
        port=$((port + 1))
    done
    echo "Serving docs on http://localhost:$port"
    mkdocs serve -a "localhost:$port"

# Build presentation slides with Marp
slides:
    #!/usr/bin/env bash
    just update-diagrams
    marp --html docs/presentation/light-clients.md -o docs/presentation/index.html

# Generate mermaid.ink URLs from .mmd files and update slides
update-diagrams:
    #!/usr/bin/env bash
    set -euo pipefail
    cd docs/presentation
    for mmd in diagrams/*.mmd; do
        name=$(basename "$mmd" .mmd)
        # Generate URL-safe base64
        encoded=$(base64 -w0 < "$mmd" | tr '+/' '-_')
        url="https://mermaid.ink/svg/$encoded"
        echo "$name: $url"
        # Update the markdown - find img with matching diagram name in comment
        sed -i "s|<!-- $name --><img src=\"[^\"]*\"|<!-- $name --><img src=\"$url\"|g" light-clients.md
    done