with import <nixpkgs> {};

let
  rust = callPackage ./nix/rust.nix {};
in mkShell {
  nativeBuildInputs = [
    rust
  ];
}
