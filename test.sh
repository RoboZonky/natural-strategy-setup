#!/bin/bash

# Download chromedriver binary if it doesn't exist
if [ ! -f "tests/chromedriver" ]; then
  echo "chromedriver binary required by tests doesn't exist. Attemping download.."
  wget --quiet --directory-prefix tests https://chromedriver.storage.googleapis.com/2.32/chromedriver_linux64.zip
  unzip tests/chromedriver_linux64.zip -d tests
fi

elm make src/Test/TestApp.elm --warn --output tests/target/testApp.html
cd tests || exit
mvn test -Dmake.elmMakeExecutable=/usr/bin/elm-make
