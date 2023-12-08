{ pkgs ? import <nixpkgs> { } }:
let
  si = pkgs.writeShellScriptBin "si" ''
    stack exec ghci app/"$1"
  '';
  srun = pkgs.writeShellScriptBin "srun" ''
    stack exec runghc app/"$1"
  '';
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
      stack
    ]))
    si
    stylish-haskell
    srun
  ];
}
