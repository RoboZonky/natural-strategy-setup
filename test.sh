#!/bin/bash
set -e # Exit immediately when some command fails

# Run unit tests
elm test

# Run Integration tests
TESTS_DIR=integration-tests
# Download chromedriver binary if it doesn't exist
if [ ! -f "${TESTS_DIR}/chromedriver" ]; then
  echo "chromedriver binary required by tests doesn't exist. Attempting download.."
  wget --quiet --directory-prefix ${TESTS_DIR} https://chromedriver.storage.googleapis.com/2.36/chromedriver_linux64.zip
  unzip ${TESTS_DIR}/chromedriver_linux64.zip -d ${TESTS_DIR}
fi

elm make src/Test/TestApp.elm --warn --output ${TESTS_DIR}/target/testApp.html
cd ${TESTS_DIR}
mvn test
