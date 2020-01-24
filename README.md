# Natural Strategy Setup

Web configuration of investment strategy for [RoboZonky](https://robozonky.github.io/).

## Run in development mode

Use [elm-live](https://github.com/wking-io/elm-live) to start the development server and open the running app in the browser.

```bash
elm-live --dir dist --open -- src/Main.elm --output dist/js/elm.js
```

## Run tests

There is [elm-test](https://package.elm-lang.org/packages/elm-explorations/test/latest/) based suite of unit tests in `tests` folder,
which can be run from the root of the project using `elm-test`.

There is Java maven based suite of selenium integration tests in `integration-tests`.
This one requires `robozonky-strategy-natural` jar of corresponding version (see integration-tests/pom.xml) to be installed in local m2 repo:

```bash
git clone git@github.com:RoboZonky/robozonky.git
cd RoboZonky
mvn clean install -DskipTests -Dgpg.skip=true
```

You also need `warp` (web server) binary which is used to serve the app for selenium tests.

```bash
stack install wai-app-static --resolver lts-14.20
```

After you have `elm-test` and `warp` binaries installed and robozonky artifacts built in your m2 repo you can run all tests using:

```bash
./test.sh
```

## Release new strategy version

Whenever there are backwards incompatible changes to the strategy format,
the commit before them needs to be tagged and testing app built from the repo based on that tag should be uploaded to [nss-strategy-compat](https://github.com/jhrcek/jhrcek.github.io/tree/master/nss-strategy-compat) for future backward compatibility testing:

```bash
NEW_VERSION=v0.6
git tag -a $NEW_VERSION -m"DESCRIBE CHANGES"
# After running tests
mkdir ~/Devel/github.com/jhrcek/jhrcek.github.io/nss-strategy-compat/$NEW_VERSION
mv integration-tests/target/testApp.html ~/Devel/github.com/jhrcek/jhrcek.github.io/nss-strategy-compat/$NEW_VERSION/index.html
# Add links to jhrcek.github.io/nss-strategy-compat/README.md
```

