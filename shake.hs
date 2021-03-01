#!/usr/bin/env stack
{- stack script
   --resolver lts-17.4
   --package process
   --package shake
-}

import Development.Shake
import Development.Shake.FilePath
import qualified System.Process as Process

-- TODO maybe check all system dependencies are present: elm, elm-live, java, mvn, sed, uglify-js, unzip, warp, wget

main :: IO ()
main = shakeArgs options $ do
  want [elmDist]

  elmDist %> \out -> do
    StdoutTrim elmFiles <- command [] "git" ["ls-files", "src"]
    need $ lines elmFiles

    cmd_ "elm make src/Main.elm --optimize --output" out
    -- Hardcode current commit hash into distribution
    -- to make it easy to link generated strategy configs to specific version of this tool
    StdoutTrim commitHash <- command [] "git" ["rev-parse", "--short", "HEAD"]
    cmd_ ("sed -i s/COMMIT_HASH_PLACEHOLDER/" <> commitHash <> "/") out

  testApp %> \_ -> do
    StdoutTrim elmFiles <- command [] "git" ["ls-files", "src"]
    need $ lines elmFiles

    cmd_ "elm make src/Test/TestApp.elm --optimize --output" testApp

  chromedriverZip %> \_ -> do
    StdoutTrim latestVersion <- command [] "curl" ["http://chromedriver.storage.googleapis.com/LATEST_RELEASE"]
    cmd_ "wget" "--quiet" "--directory-prefix" it ("http://chromedriver.storage.googleapis.com/" <> latestVersion <> "/chromedriver_linux64.zip")

  chromedriver %> \_ -> do
    need [chromedriverZip]
    cmd_ "unzip" (it </> "chromedriver_linux64.zip") "-d" it

  phony minify $ do
    need [elmDist]
    withTempFile $ \minifiedTemp -> do
      cmd_ "uglifyjs" [elmDist, "--compress", "pure_funcs=\"F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9\",pure_getters,keep_fargs=false,unsafe_comps,unsafe", "--output", minifiedTemp]
      cmd_ "uglifyjs" [minifiedTemp, "--mangle", "--output", elmDist]

  phony "dev-server" $ do
    cmd_ "elm-live --dir dist --open -- src/Main.elm --output" elmDist

  phony "test" $ do
    cmd_ "elm-test"

  phony "itest" $ do
    need ["test", elmDist, chromedriver, testApp]
    let startWarp = fromProcess <$> cmd "warp --docroot dist"
    actionBracket startWarp Process.terminateProcess $ \_ -> do
      mvn "test"

  phony "deploy-stage" $ do
    deploy "/home/jhrcek/Devel/github.com/jhrcek/jhrcek.github.io/natural-strategy-setup"

  phony "deploy-prod" $ do
    deploy "/home/jhrcek/Devel/github.com/jhrcek/robozonky.github.io/konfigurace-strategie"

  phony "clean" $ do
    cmd_ "rm" ["-rf", "elm-stuff", elmDist]
    mvn "clean"

mvn :: String -> Action ()
mvn goal = command_ [Cwd it] "mvn" [goal]

deploy :: FilePath -> Action ()
deploy targetDir = do
  need [minify]
  liftIO $ removeFiles targetDir ["*"]
  cmd_ "cp --recursive dist/." targetDir

options :: ShakeOptions
options =
  shakeOptions
    { shakeVerbosity = Verbose,
      shakeColor = True
    }

elmDist :: FilePath
elmDist = "dist/js/elm.js"

it :: FilePath
it = "integration-tests"

chromedriver :: FilePath
chromedriver = it </> "chromedriver"

chromedriverZip :: FilePath
chromedriverZip = it </> "chromedriver_linux64.zip"

testApp :: FilePath
testApp = it </> "target" </> "testApp.html"

minify :: String
minify = "minify"
