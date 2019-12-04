{ callPackage }:

let
  sources = import ./sources.nix;
  mozilla = callPackage "${sources.mozilla}/package-set.nix" {};
in mozilla.latest.rustChannels.stable.rust
