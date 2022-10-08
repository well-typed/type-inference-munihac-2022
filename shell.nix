with (import (builtins.fetchGit {
    name = "nixpkgs-unstable-2022-09-08";
    url = https://github.com/nixos/nixpkgs/;
    rev = "768e20105b7b7af05a342edd7a1cf6fcccd28378";
  }) {}).pkgs;

let
  ghc = haskell.packages.ghc924.ghcWithPackages
    (pkgs : with pkgs; [ cabal-install ]);
in
  stdenv.mkDerivation {
    name = "9.2.4";
    buildInputs = [ ghc zlib ncurses ];
    shellHook = ''
      eval $(grep export ${ghc}/bin/ghc)
      export LD_LIBRARY_PATH="${zlib}/lib:${ncurses}/lib";
      export NIX_GHC_LIBDIR=$(ghc --print-libdir)
    '';
  }
