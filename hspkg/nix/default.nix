# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal }:

cabal.mkDerivation (self: {
  pname = "hello";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  meta = {
    description = "Hello World with Nix";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})