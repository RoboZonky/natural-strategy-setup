module StrategyHashDecoding exposing
    ( invalidBase64encoding
    , invalidHashData
    , strategyFromHashTest
    , testData
    , validHashData
    )

import Base64
import Data.Filter.Conditions.Rating as Rating
import Data.Portfolio as Portfolio
import Data.Strategy as Strategy exposing (StrategyConfiguration, defaultStrategyConfiguration)
import Data.VersionedStrategy as VersionedStrategy
import Expect exposing (Expectation)
import Percentage
import Test exposing (Test, describe, test)


invalidHashData : Test
invalidHashData =
    describe "VersionedStrategy.loadStrategy - invalid inputs" <|
        List.map strategyFromHashTest testData


strategyFromHashTest : ( String, String ) -> Test
strategyFromHashTest ( urlHash, expectedError ) =
    test ("invalid hash '" ++ urlHash ++ "'") <|
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
    , ( "99;", "Unsupported strategy version 99" )
    ]


validHashData : Test
validHashData =
    describe "Strategy.strategyFromUrlHash - valid inputs" <|
        [ test "Default strategy" <|
            \() ->
                VersionedStrategy.loadStrategy "NTt7ImgiOnsiYSI6MCwiYiI6WyIwIl0sImMiOlsxXSwiZCI6WzAsMjAwXSwiZSI6WzJdLCJnMSI6MX0sImoiOltbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF1dLCJrIjp7Im8iOjB9LCJsIjp7Im0iOjB9fQ=="
                    |> withDecodedStrategy
                        (Expect.all
                            [ \( _, warnings ) -> List.isEmpty warnings |> Expect.true "There should be no warnings"
                            , \( decodedStrategy, _ ) ->
                                Strategy.strategyEqual Strategy.defaultStrategyConfiguration decodedStrategy
                                    |> Expect.true "Should decode to default strategy configuration"
                            ]
                        )
        , describe "V2 -> V3 migration"
            [ test "mobile notifications enabled - should give warning about notifications being removed" <|
                \() ->
                    VersionedStrategy.loadStrategy "Mjt7ImgiOnsiYSI6MCwiYiI6WyIwIl0sImMiOlsxXSwiZCI6WzAsMjAwXSwiZSI6WzJdLCJmIjpbMV0sImciOnsiYSI6MSwiYiI6eyJ2IjozLCJ3Ijo1fX0sImcxIjoxfSwiaiI6W1swLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXV0sImsiOnsibyI6MH0sImwiOnsibSI6MH19"
                        |> withDecodedStrategy
                            (Expect.all
                                [ \( _, warnings ) -> warnings |> Expect.equal [ "Vaše strategie měla nastaveno Potvrzení investic mobilem, které muselo být odstraněno." ]
                                , \( decodedStrategy, _ ) ->
                                    Strategy.strategyEqual Strategy.defaultStrategyConfiguration decodedStrategy
                                        |> Expect.true "Should decode to default strategy configuration"
                                ]
                            )
            , test "default strategy = notification disabled" <|
                \() ->
                    VersionedStrategy.loadStrategy "Mjt7ImgiOnsiYSI6MCwiYiI6WyIwIl0sImMiOlsxXSwiZCI6WzAsMjAwXSwiZSI6WzJdLCJmIjpbMV0sImciOnsiYSI6MH0sImcxIjoxfSwiaiI6W1swLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXV0sImsiOnsibyI6MH0sImwiOnsibSI6MH19"
                        |> withDecodedStrategy (\( _, warnings ) -> List.isEmpty warnings |> Expect.true "There should be no warnings")
            ]
        , describe "V3 -> V4 migration"
            [ test "target balance set - should give warning about setting removed" <|
                \() ->
                    VersionedStrategy.loadStrategy "Mzt7ImgiOnsiYSI6MCwiYiI6WyIwIl0sImMiOlsxXSwiZCI6WzAsMjAwXSwiZSI6WzJdLCJmIjpbMiwxMDAwXSwiZzEiOjF9LCJqIjpbWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdXSwiayI6eyJvIjowfSwibCI6eyJtIjowfX0="
                        |> withDecodedStrategy
                            (Expect.all
                                [ \( _, warnings ) -> warnings |> Expect.equal [ "Vaše strategie měla nastaveno omezení investic na základě disponibilního zůstatku\n\"Investovat pouze pokud disponibilní zůstatek přesáhne 1000 Kč.\"\n, které muselo být odstraněno." ]
                                , \( decodedStrategy, _ ) ->
                                    Strategy.strategyEqual Strategy.defaultStrategyConfiguration decodedStrategy
                                        |> Expect.true "Should decode to default strategy configuration"
                                ]
                            )
            , test "Default V3 strategy - target balance not set - should give no warnings" <|
                \() ->
                    VersionedStrategy.loadStrategy "Mzt7ImgiOnsiYSI6MCwiYiI6WyIwIl0sImMiOlsxXSwiZCI6WzAsMjAwXSwiZSI6WzJdLCJmIjpbMV0sImcxIjoxfSwiaiI6W1swLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXV0sImsiOnsibyI6MH0sImwiOnsibSI6MH19"
                        |> withDecodedStrategy
                            (Expect.all
                                [ \( _, warnings ) -> warnings |> Expect.equal []
                                , \( decodedStrategy, _ ) ->
                                    Strategy.strategyEqual Strategy.defaultStrategyConfiguration decodedStrategy
                                        |> Expect.true "Should decode to default strategy configuration"
                                ]
                            )
            ]
        , describe "V4 -> V5 migration"
            [ test "custom portfolio using ranges - should give warning about portfolio simplification" <|
                \() ->
                    VersionedStrategy.loadStrategy "NDt7ImkiOltbMywxMF0sWzEzLDIwXSxbMTksMzBdLFsyMSwyMV0sWzE5LDE5XSxbMTEsMTFdLFs3LDddLFs1LDVdLFsyLDJdLFsxLDFdLFswLDEwXV0sImgiOnsiYSI6MywiYiI6WyIwIl0sImMiOlsxXSwiZCI6WzAsMjAwXSwiZSI6WzJdLCJnMSI6MX0sImoiOltbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF1dLCJrIjp7Im8iOjB9LCJsIjp7Im0iOjB9fQ=="
                        |> withDecodedStrategy
                            (Expect.all
                                [ \( _, warnings ) ->
                                    warnings
                                        |> Expect.equal
                                            -- TODO these should be sorted by rating and more readable
                                            [ "Vaše strategie měla nastavenu vámi definovanou strukturu portfolia, která musela být zjednodušena:\n • 2,99 % p.a. z rozsahu '3 až 10%' na '10%'\n • 3,99 % p.a. z rozsahu '13 až 20%' na '20%'\n • 4,99 % p.a. z rozsahu '19 až 30%' na '30%'\n • 19,99 % p.a. z rozsahu '0 až 10%' na '10%'"
                                            ]
                                , \( decodedStrategy, _ ) ->
                                    let
                                        expectedStrategy : Strategy.StrategyConfiguration
                                        expectedStrategy =
                                            defaultStrategyConfiguration
                                                |> Strategy.setPortfolio Portfolio.UserDefined
                                                |> Strategy.setPortfolioSharePercentage Rating.AAAAAA (Percentage.SetValue 10)
                                                |> Strategy.setPortfolioSharePercentage Rating.AAAAA (Percentage.SetValue 20)
                                                |> Strategy.setPortfolioSharePercentage Rating.AAAA (Percentage.SetValue 30)
                                                |> Strategy.setPortfolioSharePercentage Rating.D (Percentage.SetValue 10)
                                    in
                                    Strategy.strategyEqual expectedStrategy decodedStrategy
                                        |> Expect.true "Should decode to expected Strategy"
                                ]
                            )
            , test "custom portfolio without ranges - should not give any warnings" <|
                \() ->
                    VersionedStrategy.loadStrategy "NDt7ImkiOltbMywzXSxbMTMsMTNdLFsxOSwxOV0sWzIxLDIxXSxbMTksMTldLFsxMSwxMV0sWzcsN10sWzUsNV0sWzIsMl0sWzEsMV0sWzAsMF1dLCJoIjp7ImEiOjMsImIiOlsiMCJdLCJjIjpbMV0sImQiOlswLDIwMF0sImUiOlsyXSwiZzEiOjF9LCJqIjpbWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdXSwiayI6eyJvIjowfSwibCI6eyJtIjowfX0="
                        |> withDecodedStrategy
                            (Expect.all
                                [ \( _, warnings ) -> warnings |> Expect.equal []
                                , \( decodedStrategy, _ ) ->
                                    let
                                        expectedStrategy =
                                            Strategy.setPortfolio Portfolio.UserDefined defaultStrategyConfiguration
                                    in
                                    Strategy.strategyEqual expectedStrategy decodedStrategy
                                        |> Expect.true "Should decode almost default strategy with UserDefined portfolio"
                                ]
                            )
            ]
        ]



--|> Strategy.setPortfolioSharePercentage


withDecodedStrategy :
    (( StrategyConfiguration, List String ) -> Expectation) -- What to assert about successfully decoded strategy
    -> Result String ( StrategyConfiguration, List String )
    -> Expectation
withDecodedStrategy buildExpectation decodingResult =
    case decodingResult of
        Ok ok ->
            buildExpectation ok

        Err error ->
            Expect.fail <| "Failed to decode strategy: " ++ error
