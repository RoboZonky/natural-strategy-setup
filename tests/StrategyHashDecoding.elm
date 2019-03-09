module StrategyHashDecoding exposing
    ( invalidBase64encoding
    , invalidHashData
    , strategyFromHashTest
    , testData
    , validHashData
    )

import Base64
import Data.Strategy as Strategy
import Data.VersionedStrategy as VersionedStrategy
import Expect
import Test exposing (Test, describe, test)


invalidHashData : Test
invalidHashData =
    describe "VersionedStrategy.loadStrategy - invalid inputs" <|
        List.map strategyFromHashTest testData


strategyFromHashTest : ( String, String ) -> Test
strategyFromHashTest ( urlHash, expectedError ) =
    test ("invadlid hash '" ++ urlHash ++ "'") <|
        \() ->
            VersionedStrategy.loadStrategy (Base64.encode urlHash)
                |> Expect.equal (Err expectedError)


invalidBase64encoding : Test
invalidBase64encoding =
    test "VersionedStrategy.loadStrategy should return error with invalid Base64 encoded strings" <|
        \() ->
            VersionedStrategy.loadStrategy "šmankote!"
                |> Expect.equal (Err "Invalid base64")


testData : List ( String {- Plain data (before base64 encoded) from URL -}, String {- expected error -} )
testData =
    [ ( "", "Unexpected number of semicolon separated things in " )
    , ( ";;", "Unexpected number of semicolon separated things in ;;" )
    , ( ";", "Failed to read strategy version from " )
    , ( "a;", "Failed to read strategy version from a" )
    , ( "0;", "Unsupported strategy version 0" )
    , ( "3;", "Unsupported strategy version 3" )
    ]


validHashData : Test
validHashData =
    describe "Strategy.strategyFromUrlHash - valid inputs" <|
        [ test "default strategy" <|
            \() ->
                case VersionedStrategy.loadStrategy "Mjt7ImgiOnsiYSI6MCwiYiI6WyIwIl0sImMiOlsxXSwiZCI6WzIwMCwyMDBdLCJlIjpbMl0sImYiOlsxXSwiZyI6eyJhIjowfSwiZzEiOjF9LCJpIjpbWzMsM10sWzYsNl0sWzE2LDE2XSxbMjUsMjVdLFsyMCwyMF0sWzE1LDE1XSxbMTUsMTVdLFswLDBdXSwiaiI6W1syMDAsMjAwXSxbMjAwLDIwMF0sWzIwMCwyMDBdLFsyMDAsMjAwXSxbMjAwLDIwMF0sWzIwMCwyMDBdLFsyMDAsMjAwXSxbMjAwLDIwMF1dLCJrIjp7Im8iOjB9LCJsIjp7Im0iOjB9fQ==" of
                    Ok ( decodedStrategy, warnings ) ->
                        Strategy.strategyEqual Strategy.defaultStrategyConfiguration decodedStrategy |> Expect.true "Should decode to default strategy configuration"

                    Err e ->
                        Expect.fail <| "Failed to decode strategy: " ++ e
        ]
