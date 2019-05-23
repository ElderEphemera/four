(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    four = ./game;
  };

  shells = {
    ghc = ["four"];
    ghcjs = ["four"];
  };
})