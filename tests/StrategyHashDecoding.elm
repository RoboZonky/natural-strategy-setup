module StrategyHashDecoding exposing
    ( invalidBase64encoding
    , invalidHashData
    , strategyFromHashTest
    , testData
    , validHashData
    )

import Base64
import Data.Filter.Conditions.Rating as Rating
import Data.Migration.Strategy.V6 exposing (investmentSizeWarning)
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
                VersionedStrategy.loadStrategy "Njt7ImgiOnsiYSI6MCwiYiI6WyIwIl0sImMiOlsxXSwiZCI6MjAwLCJkMSI6MjAwLCJnMSI6MX0sImoiOlsyMDAsMjAwLDIwMCwyMDAsMjAwLDIwMCwyMDAsMjAwLDIwMCwyMDAsMjAwXSwiajEiOlsyMDAsMjAwLDIwMCwyMDAsMjAwLDIwMCwyMDAsMjAwLDIwMCwyMDAsMjAwXSwiayI6eyJvIjowfSwibCI6eyJtIjowfX0="
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
                                [ \( _, warnings ) ->
                                    warnings
                                        |> Expect.equal
                                            [ "strategie měla nastaveno Potvrzení investic mobilem, které muselo být odstraněno."
                                            , investmentSizeWarning
                                            ]
                                , \( decodedStrategy, _ ) ->
                                    Strategy.strategyEqual Strategy.defaultStrategyConfiguration decodedStrategy
                                        |> Expect.true "Should decode to default strategy configuration"
                                ]
                            )
            , test "default strategy = notification disabled" <|
                \() ->
                    VersionedStrategy.loadStrategy "Mjt7ImgiOnsiYSI6MCwiYiI6WyIwIl0sImMiOlsxXSwiZCI6WzAsMjAwXSwiZSI6WzJdLCJmIjpbMV0sImciOnsiYSI6MH0sImcxIjoxfSwiaiI6W1swLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXV0sImsiOnsibyI6MH0sImwiOnsibSI6MH19"
                        |> withDecodedStrategy (\( _, warnings ) -> warnings |> Expect.equal [ investmentSizeWarning ])
            ]
        , describe "V3 -> V4 migration"
            [ test "target balance set - should give warning about setting removed" <|
                \() ->
                    VersionedStrategy.loadStrategy "Mzt7ImgiOnsiYSI6MCwiYiI6WyIwIl0sImMiOlsxXSwiZCI6WzAsMjAwXSwiZSI6WzJdLCJmIjpbMiwxMDAwXSwiZzEiOjF9LCJqIjpbWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdXSwiayI6eyJvIjowfSwibCI6eyJtIjowfX0="
                        |> withDecodedStrategy
                            (Expect.all
                                [ \( _, warnings ) ->
                                    warnings
                                        |> Expect.equal
                                            [ "strategie měla nastaveno omezení investic na základě disponibilního zůstatku\n\"Investovat pouze pokud disponibilní zůstatek přesáhne 1000 Kč.\"\n, které muselo být odstraněno."
                                            , investmentSizeWarning
                                            ]
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
                                [ \( _, warnings ) -> warnings |> Expect.equal [ investmentSizeWarning ]
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
                                            [ "strategie měla nastavenu vámi definovanou strukturu portfolia, která musela být zjednodušena:"
                                            , "\u{00A0}\u{00A0}Požadovaný podíl investovaný do půjček s úročením 2,99 % p.a. byl změněn z rozsahu '3 až 10%' na '10%'"
                                            , "\u{00A0}\u{00A0}Požadovaný podíl investovaný do půjček s úročením 3,99 % p.a. byl změněn z rozsahu '13 až 20%' na '20%'"
                                            , "\u{00A0}\u{00A0}Požadovaný podíl investovaný do půjček s úročením 4,99 % p.a. byl změněn z rozsahu '19 až 30%' na '30%'"
                                            , "\u{00A0}\u{00A0}Požadovaný podíl investovaný do půjček s úročením 19,99 % p.a. byl změněn z rozsahu '0 až 10%' na '10%'"
                                            , investmentSizeWarning
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
                                                |> Strategy.setPortfolioSharePercentage Rating.B (Percentage.SetValue 2)
                                                |> Strategy.setPortfolioSharePercentage Rating.C (Percentage.SetValue 1)
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
                                [ \( _, warnings ) -> warnings |> Expect.equal [ investmentSizeWarning ]
                                , \( decodedStrategy, _ ) ->
                                    let
                                        expectedStrategy =
                                            defaultStrategyConfiguration
                                                |> Strategy.setPortfolio Portfolio.UserDefined
                                                |> Strategy.setPortfolioSharePercentage Rating.B (Percentage.SetValue 2)
                                                |> Strategy.setPortfolioSharePercentage Rating.C (Percentage.SetValue 1)
                                    in
                                    Strategy.strategyEqual expectedStrategy decodedStrategy
                                        |> Expect.true "Should decode almost default strategy with UserDefined portfolio"
                                ]
                            )
            ]
        , describe "V5 -> V6 migration"
            [ test "Should give warning about removal of nondefault InvestmentShare" <|
                \() ->
                    VersionedStrategy.loadStrategy "NTt7ImgiOnsiYSI6MCwiYiI6WyIwIl0sImMiOlsxXSwiZCI6WzAsMjAwXSwiZSI6WzEsMTBdLCJnMSI6MX0sImoiOltbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF1dLCJrIjp7Im8iOjB9LCJsIjp7Im0iOjB9fQ=="
                        |> withDecodedStrategy
                            (Expect.all
                                [ \( _, warnings ) ->
                                    List.member "strategie obsahovala nastavení 'Investovat maximálně 10 % výše úvěru.', které muselo být odstraněno." warnings
                                        |> Expect.true "should contain warning"
                                , \( decodedStrategy, _ ) ->
                                    Strategy.strategyEqual defaultStrategyConfiguration decodedStrategy
                                        |> Expect.true "Should decode to default strategy configuration"
                                ]
                            )
            , test "custom portfolio without ranges - should not give any warnings" <|
                \() ->
                    VersionedStrategy.loadStrategy "NDt7ImkiOltbMywzXSxbMTMsMTNdLFsxOSwxOV0sWzIxLDIxXSxbMTksMTldLFsxMSwxMV0sWzcsN10sWzUsNV0sWzIsMl0sWzEsMV0sWzAsMF1dLCJoIjp7ImEiOjMsImIiOlsiMCJdLCJjIjpbMV0sImQiOlswLDIwMF0sImUiOlsyXSwiZzEiOjF9LCJqIjpbWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdLFswLDIwMF0sWzAsMjAwXSxbMCwyMDBdXSwiayI6eyJvIjowfSwibCI6eyJtIjowfX0="
                        |> withDecodedStrategy
                            (Expect.all
                                [ \( _, warnings ) -> warnings |> Expect.equal [ investmentSizeWarning ]
                                , \( decodedStrategy, _ ) ->
                                    let
                                        expectedStrategy =
                                            defaultStrategyConfiguration
                                                |> Strategy.setPortfolio Portfolio.UserDefined
                                                |> Strategy.setPortfolioSharePercentage Rating.B (Percentage.SetValue 2)
                                                |> Strategy.setPortfolioSharePercentage Rating.C (Percentage.SetValue 1)
                                    in
                                    Strategy.strategyEqual expectedStrategy decodedStrategy
                                        |> Expect.true "Should decode almost default strategy with UserDefined portfolio"
                                ]
                            )
            ]
        ]


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
