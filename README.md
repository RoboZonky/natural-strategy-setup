# Natural Strategy Setup

Web configuration of investment strategy for [RoboZonky](https://robozonky.github.io/).

## Running in development mode

Use [elm-live](https://github.com/wking-io/elm-live) to start the development server and open the running app in the browser.

```bash
elm-live --dir dist --start-page dist/index.html --open -- src/Main.elm --output dist/js/elm.js
```

## Running tests

There is [elm-test](https://package.elm-lang.org/packages/elm-explorations/test/latest/) based suite of unit tests in `tests` folder,
which can be run from the root of the project using `elm-test`.

There is Java maven based suite of selenium integration tests in `integration-tests`.
This one requires `robozonky-strategy-natural` jar of corresponding version (see integration-tests/pom.xml) to be installed in local m2 repo:

```bash
git clone git@github.com:RoboZonky/robozonky.git
cd RoboZonky
mvn clean install -DskipTests -Dgpg.skip=true
```

After you have `elm-test` installed and robozonky artifacts built in your m2 repo you can run all tests using:

```bash
./test.sh
```
