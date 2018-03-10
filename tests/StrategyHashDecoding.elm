module StrategyHashDecoding exposing (..)

import Base64
import Data.Strategy as Strategy
import Expect
import Test exposing (Test, describe, test)


invalidHashData : Test
invalidHashData =
    describe "Strategy.strategyFromUrlHash - invalid inputs" <|
        List.map strategyFromHashTest testData


strategyFromHashTest : ( String, String ) -> Test
strategyFromHashTest ( urlHash, expectedError ) =
    test ("invadlid hash '" ++ urlHash ++ "'") <|
        \() ->
            Strategy.strategyFromUrlHash (Base64.encode urlHash)
                |> Expect.equal (Err expectedError)


testData : List ( String {- Plain data (before base64 encoded) from URL -}, String {- expected error -} )
testData =
    [ ( "", "Unexpected number of semicolon separated things in " )
    , ( ";;", "Unexpected number of semicolon separated things in ;;" )
    , ( ";", "Failed to read strategy version from " )
    , ( "a;", "Failed to read strategy version from a" )
    , ( "0;", "Unsupported strategy version 0" )
    , ( "2;", "Unsupported strategy version 2" )
    ]


validHashData : Test
validHashData =
    describe "Strategy.strategyFromUrlHash - valid inputs" <|
        [ test "default strategy" <|
            \() ->
                Strategy.strategyFromUrlHash "MTt7ImgiOnsiYSI6MCwiYiI6WyIwIl0sImMiOlsxXSwiZCI6WzIwMCwyMDBdLCJlIjpbMl0sImYiOlsxXSwiZyI6W119LCJpIjpbWzMsM10sWzYsNl0sWzE2LDE2XSxbMjUsMjVdLFsyMCwyMF0sWzE1LDE1XSxbMTUsMTVdLFswLDBdXSwiaiI6W1syMDAsMjAwXSxbMjAwLDIwMF0sWzIwMCwyMDBdLFsyMDAsMjAwXSxbMjAwLDIwMF0sWzIwMCwyMDBdLFsyMDAsMjAwXSxbMjAwLDIwMF1dLCJrIjp7Im8iOjB9LCJsIjp7Im0iOjB9fQ=="
                    |> Result.map (Strategy.strategyEqual Strategy.defaultStrategyConfiguration)
                    |> Result.withDefault False
                    |> Expect.true "Should decode to default strategy configuration"
        ]
