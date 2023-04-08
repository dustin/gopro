{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  buildInputs = [
    stack
    sqlite-interactive
    ffmpeg
    stylish-haskell
    hlint
    ghcid
    haskell-language-server
  ];

  shellHook = ''
    # ...
  '';
}
