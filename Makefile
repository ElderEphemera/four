native:
	nix-build -o result-native -A build.native

web:
	nix-build -o result-web -A build.web

zip:
	nix-build -o result-zip -A build.zip

android:
	nix-build -o result-android -A build.android

.PHONY: native web zip android
