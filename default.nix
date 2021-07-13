let
  platform = import ./reflex-platform { config.android_sdk.accept_license = true; };
  inherit (platform) nixpkgs;

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

    overrides = self: super: {
      # Clay is broken because of testsuite dependencies.
      clay = with pkgs.haskell.lib; markUnbroken (dontCheck super.clay);
    };
  });

  native = project.ghc.four;
  rawWeb = project.ghcjs.four;
  android = project.android.four;

  webDir = "$src/bin/four.jsexe";
  webFiles = nixpkgs.lib.strings.concatStringsSep " "
    [ "index.html" "rts.js" "lib.js" "out.js" "runmain.js" ];
  web = nixpkgs.runCommand "four-zip" {
    src = rawWeb;
  } ''
    mkdir -p $out
    cd ${webDir}
    cp ${webFiles} $out
  '';

  zip = nixpkgs.runCommand "four-zip" {
    src = web;
    buildInputs = [ nixpkgs.zip ];
  } ''
    mkdir -p $out
    zip $out/four.zip $src/*
  '';

  build = { inherit native rawWeb web zip android; };
  shells = project.shells;
in { inherit build shells; }