#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nixos-generators

set -efu
WD=$(dirname "$0")

nixos-generate -c "$WD"/vm.nix -f vm-nogui --run
