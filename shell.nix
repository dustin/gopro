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
  ];

  shellHook = ''
    # ...
  '';
}
