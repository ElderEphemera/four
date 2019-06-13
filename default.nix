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
  });

  native = project.ghc.four;
  web = project.ghcjs.four;
  android = project.android.four;

  webDir = "$src/bin/four.jsexe";
  webFiles = [ "index.html" "rts.js" "lib.js" "out.js" "runmain.js" ];
  zipFiles = nixpkgs.lib.strings.concatStringsSep " " webFiles;
  zip = nixpkgs.runCommand "four-zip" {
    src = web;
    buildInputs = [ nixpkgs.zip ];
  } ''
    mkdir -p $out
    cd ${webDir}
    zip $out/four.zip ${zipFiles}
  '';

  build = { inherit native web zip android; };
  shells = project.shells;
in { inherit build shells; }