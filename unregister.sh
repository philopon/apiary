#!/bin/bash

set -a pkgs

for p in `cat submodules`; do
  pkgs=("$p" "${pkgs[@]}")
done

for p in ${pkgs[@]}; do
  ghc-pkg unregister $p
done

ghc-pkg unregister apiary
