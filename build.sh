#!/bin/bash
elm make --warn src/Main.elm --output dist/js/elm.js

# Hardcode current commit hash into distribution
# to make it easy to link generated strategy configs to specific version of this tool
COMMIT_HASH=`git rev-parse --short HEAD`
sed -i "s/COMMIT_HASH_PLACEHOLDER/$COMMIT_HASH/" dist/js/elm.js
