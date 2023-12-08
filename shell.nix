{ pkgs ? import <nixpkgs> { } }:
let
  # Add autocomplete to si
  si = pkgs.writeShellScriptBin "si" ''
    stack exec ghci "$1"
  '';
  srun = pkgs.writeShellScriptBin "srun" ''
    stack exec runghc "$1"
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
