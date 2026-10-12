{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  buildInputs = [
    sqlite-interactive
    ffmpeg
    stylish-haskell
    hlint
    niv
    ghcid
    haskell-language-server
    haskellPackages.tasty-discover
    cabal-install
    haskell.compiler.ghc9124
    hpack
    zlib
    postgresql
  ];

  shellHook = ''
    # ...
  '';
}
