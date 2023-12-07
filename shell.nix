{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
      vector
    ]))
    stylish-haskell
  ];
}
