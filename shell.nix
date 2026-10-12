{ pkgs ? import <nixpkgs> { } }:
with pkgs;
let
  hpkgs = haskell.packages.ghc967.override {
    overrides = self: super: {
      gopro-plus = haskell.lib.dontCheck (haskell.lib.unmarkBroken super.gopro-plus);
      gpmf       = haskell.lib.dontCheck (haskell.lib.unmarkBroken super.gpmf);
    };
  };
  ghcWithDeps = hpkgs.ghcWithPackages (p: with p; [
    aeson
    amazonka
    amazonka-core
    amazonka-lambda
    amazonka-s3
    amazonka-sqs
    async
    bytestring
    cache
    effectful
    effectful-th
    clock
    conduit
    conduit-extra
    containers
    dimensional
    directory
    exceptions
    extra
    filepath
    foldl
    generic-deriving
    generic-lens
    geodetics
    gopro-plus
    gpmf
    hasql
    hasql-pool
    hasql-th
    hasql-transaction
    hsexif
    http-conduit
    http-types
    lens
    lens-aeson
    monad-logger
    monad-loops
    mtl
    optparse-applicative
    pathwalk
    process
    raw-strings-qq
    resourcet
    retry
    safe
    scientific
    scotty
    semigroupoids
    shelly
    split
    sqlite-simple
    stm
    text
    these
    time
    tomland
    transformers
    unix
    unliftio
    # test deps
    QuickCheck
    quickcheck-instances
    tasty
    tasty-discover
    tasty-hunit
    tasty-quickcheck
  ]);
in
mkShell {
  buildInputs = [
    ghcWithDeps
    sqlite-interactive
    ffmpeg
    stylish-haskell
    hlint
    niv
    ghcid
    hpkgs.haskell-language-server
    hpkgs.tasty-discover
    cabal-install
    hpack
    zlib
    postgresql
  ];

  shellHook = ''
    # ...
  '';
}
