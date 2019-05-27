#! /bin/sh -

nix-build -A ghcjs.four

mkdir -p result-zip

zip result-zip/four.zip \
    result/bin/four.jsexe/index.html \
    result/bin/four.jsexe/rts.js \
    result/bin/four.jsexe/lib.js \
    result/bin/four.jsexe/out.js \
    result/bin/four.jsexe/runmain.js
