(import ./reflex-platform {
  config.android_sdk.accept_license = true;
}).project ({ pkgs, ... }: {
  packages = {
    four = ./game;
  };

  shells = {
    ghc = ["four"];
    ghcjs = ["four"];
  };

  android.four = {
    executableName = "four";
    applicationId = "io.github.elderephemera.four";
    displayName = "Four";
    resources = ./res;
  };
})