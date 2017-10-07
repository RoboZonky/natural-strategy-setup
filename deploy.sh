#!/usr/bin/env bash
#DEPLOYMENT_DIR=../../jhrcek/jhrcek.github.io/natural-strategy-setup
DEPLOYMENT_DIR=../../jhrcek/robozonky/konfigurace-strategie
rm --recursive --force $DEPLOYMENT_DIR
cp --recursive dist $DEPLOYMENT_DIR

echo "Minifying js file"
rm $DEPLOYMENT_DIR/js/elm.js
uglifyjs dist/js/elm.js --compress --mangle --output $DEPLOYMENT_DIR/js/elm.js
