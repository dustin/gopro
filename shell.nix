{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  buildInputs = [
    stack
    sqlite-interactive
    ffmpeg
    stylish-haskell
    hlint
    niv
    ghcid
    haskell-language-server
    haskellPackages.tasty-discover
    cabal-install
    ghc
    zlib
  ];

  shellHook = ''
    # ...
  '';
}
