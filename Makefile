native:
	nix-build -o result-native -A build.native

web:
	nix-build -o result-web -A build.web

zip: web
	mkdir -p result-zip
	zip result-zip/four.zip \
	    result-web/bin/four.jsexe/index.html \
	    result-web/bin/four.jsexe/rts.js \
	    result-web/bin/four.jsexe/lib.js \
	    result-web/bin/four.jsexe/out.js \
	    result-web/bin/four.jsexe/runmain.js

android:
	nix-build -o result-android -A build.android

.PHONY: native web zip android
