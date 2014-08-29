#!/bin/bash

set -a pkgs

for p in `cat submodules`; do
  pkgs=("$p" "${pkgs[@]}")
done

for p in ${pkgs[@]}; do
  cabal sandbox hc-pkg unregister $p
done

cabal sandbox hc-pkg unregister apiary
