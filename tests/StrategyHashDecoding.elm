module StrategyHashDecoding exposing (invalidHashData, strategyFromHashTest, testData, validHashData)

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
                case Strategy.strategyFromUrlHash "MTt7ImgiOnsiYSI6MCwiYiI6WyIwIl0sImMiOlsxXSwiZCI6WzIwMCwyMDBdLCJlIjpbMl0sImYiOlsxXSwiZyI6eyJhIjowfX0sImkiOltbMywzXSxbNiw2XSxbMTYsMTZdLFsyNSwyNV0sWzIwLDIwXSxbMTUsMTVdLFsxNSwxNV0sWzAsMF1dLCJqIjpbWzIwMCwyMDBdLFsyMDAsMjAwXSxbMjAwLDIwMF0sWzIwMCwyMDBdLFsyMDAsMjAwXSxbMjAwLDIwMF0sWzIwMCwyMDBdLFsyMDAsMjAwXV0sImsiOnsibyI6MH0sImwiOnsibSI6MH19" of
                    Ok decodedStrategy ->
                        Strategy.strategyEqual Strategy.defaultStrategyConfiguration decodedStrategy |> Expect.true "Should decode to default strategy configuration"

                    Err e ->
                        Expect.fail <| "Failed to decode strategy: " ++ e
        ]
