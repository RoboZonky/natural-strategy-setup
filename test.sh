#!/bin/bash
set -euxo pipefail

# Run unit tests
elm-test

# Run Integration tests
TESTS_DIR=integration-tests
# Download chromedriver binary if it doesn't exist
if [ ! -f "${TESTS_DIR}/chromedriver" ]; then
  echo "Downloading latest chromedriver binary"
  VERSION=$(curl http://chromedriver.storage.googleapis.com/LATEST_RELEASE)
  wget --quiet --directory-prefix ${TESTS_DIR} "http://chromedriver.storage.googleapis.com/$VERSION/chromedriver_linux64.zip"
  unzip ${TESTS_DIR}/chromedriver_linux64.zip -d ${TESTS_DIR}
fi

./build.sh
# Run NSS app in web server
warp --docroot dist &
WARP_PID=$!
elm make src/Test/TestApp.elm --optimize --output ${TESTS_DIR}/target/testApp.html
cd ${TESTS_DIR}
mvn test || true
kill $WARP_PID
