let
  platform = import ./reflex-platform { config.android_sdk.accept_license = true; };
  project = platform.project ({ pkgs, ... }: {
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
  });

  native = project.ghc.four;
  web = project.ghcjs.four;
  android = project.android.four;

  build = { inherit native web android; };
  shells = project.shells;
in { inherit build shells; }