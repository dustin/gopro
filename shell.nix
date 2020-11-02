with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "haskell";
  buildInputs = [
    stack
    sqlite-interactive
    ffmpeg
    stylish-haskell
    hlint
  ];
}
