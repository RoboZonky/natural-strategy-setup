#!/bin/bash

elm make src/Test/TestApp.elm --warn --output tests/target/testApp.html
cd tests || exit
mvn test -Dmake.elmMakeExecutable=/usr/bin/elm-make
