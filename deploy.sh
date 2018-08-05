#!/usr/bin/env bash
#DEPLOYMENT_DIR=../../jhrcek/jhrcek.github.io/natural-strategy-setup
DEPLOYMENT_DIR=../robozonky.github.io/konfigurace-strategie
rm --recursive --force $DEPLOYMENT_DIR
cp --recursive dist $DEPLOYMENT_DIR

if ! command -v uglifyjs >/dev/null 2>&1 ; then
  echo "uglifyjs is not installed. Installing it with 'sudo npm install --global uglify-js'"
  sudo npm install --global uglify-js
fi

echo "Minifying js file"
rm $DEPLOYMENT_DIR/js/elm.js
uglifyjs dist/js/elm.js --compress --mangle --output $DEPLOYMENT_DIR/js/elm.js
