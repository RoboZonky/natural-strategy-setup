module Data.PortfolioStructure exposing
    ( PortfolioStructure
    , balanced
    , balancedShares
    , conservative
    , conservativeShares
    , decoder
    , decoderFromPortfolio
    , encode
    , fromPercentageList
    , percentageSum
    , portfolioStructureEqual
    , progressive
    , progressiveShares
    , renderPortfolioStructure
    , validate
    )

import Data.Filter.Conditions.Rating as Rating exposing (Rating(..))
import Data.Portfolio exposing (Portfolio(..))
import Data.Validate as Validate
import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Percentage exposing (Percentage)
import Util


type alias PortfolioStructure =
    AnyDict Int Rating Percentage


renderRatingPercentage : ( Rating, Percentage ) -> String
renderRatingPercentage ( rating, percentage ) =
    "Prostředky úročené "
        ++ Rating.showInterestPercent rating
        ++ " mají tvořit "
        ++ renderPercentage percentage
        ++ " % aktuální zůstatkové částky."


renderPercentage : Percentage -> String
renderPercentage =
    String.fromInt << Percentage.toInt


renderPortfolioStructure : Portfolio -> PortfolioStructure -> String
renderPortfolioStructure portfolio portfolioStructure =
    case portfolio of
        UserDefined ->
            Rating.ratingDictToList portfolioStructure
                -- Only render percentage in the config when value > 0
                |> List.filter (\( _, percentage ) -> Percentage.toInt percentage > 0)
                |> List.map renderRatingPercentage
                |> Util.renderNonemptySection "\n- Úprava struktury portfolia"

        _ ->
            ""


percentageSum : PortfolioStructure -> Int
percentageSum portfolioStructure =
    portfolioStructure
        |> Dict.Any.foldr (\_ percentage sumAcc -> sumAcc + Percentage.toFloat percentage) 0
        |> Basics.round


validate : PortfolioStructure -> List String
validate portfolioStructure =
    let
        sumOfPercentages =
            percentageSum portfolioStructure
    in
    Validate.validate (sumOfPercentages < 100) <|
        "Součet podílů nesmí být menší než 100% (teď je "
            ++ String.fromInt sumOfPercentages
            ++ "%)"


portfolioStructureEqual : PortfolioStructure -> PortfolioStructure -> Bool
portfolioStructureEqual ps1 ps2 =
    let
        toComparable =
            Dict.Any.values << Dict.Any.map (\_ percentage -> Percentage.toInt percentage)
    in
    toComparable ps1 == toComparable ps2



-- JSON


encode : PortfolioStructure -> Value
encode portfolioStructure =
    Rating.ratingDictToList portfolioStructure
        |> Encode.list
            (\( _
                {- assuming that rating is always sorted in order of rating's toInterestPercent,
                   so just encoding slider states
                -}
              , percentage
              )
             ->
                encodePercentage percentage
            )


decoder : Decoder PortfolioStructure
decoder =
    Decode.list percentageDecoder
        |> Decode.map fromPercentageList
        |> Decode.andThen
            (\res ->
                case res of
                    Ok portfolioStructure ->
                        Decode.succeed portfolioStructure

                    Err e ->
                        Decode.fail e
            )


fromPercentageList : List Percentage -> Result String PortfolioStructure
fromPercentageList percentageList =
    let
        expectedLength =
            List.length Rating.allRatings

        actualLength =
            List.length percentageList
    in
    if expectedLength == actualLength then
        Ok <| Rating.initRatingDict <| List.map2 Tuple.pair Rating.allRatings percentageList

    else
        Err <|
            "Dočlo k chybě při načítání struktury portfolia: čekal jsem "
                ++ String.fromInt expectedLength
                ++ " čísel, ale dostal jsem "
                ++ String.fromInt actualLength


encodePercentage : Percentage -> Value
encodePercentage =
    Percentage.toInt >> Encode.int


percentageDecoder : Decoder Percentage
percentageDecoder =
    Decode.map Percentage.fromInt Decode.int


decoderFromPortfolio : Portfolio -> Decoder PortfolioStructure
decoderFromPortfolio portfolio =
    case portfolio of
        Conservative ->
            Decode.succeed conservative

        Balanced ->
            Decode.succeed balanced

        Progressive ->
            Decode.succeed progressive

        UserDefined ->
            Decode.field "i" decoder


conservative : PortfolioStructure
conservative =
    initPortfolioStructure conservativeShares


balanced : PortfolioStructure
balanced =
    initPortfolioStructure balancedShares


progressive : PortfolioStructure
progressive =
    initPortfolioStructure progressiveShares


initPortfolioStructure : List ( Rating, Float ) -> PortfolioStructure
initPortfolioStructure =
    List.map (Tuple.mapSecond Percentage.fromFloat)
        >> Rating.initRatingDict


conservativeShares : List ( Rating, Float )
conservativeShares =
    [ ( AAAAAA, 3 )
    , ( AAAAA, 13 )
    , ( AAAA, 19 )
    , ( AAA, 21 )
    , ( AAE, 19 )
    , ( AA, 11 )
    , ( AE, 7 )
    , ( A, 5 )
    , ( B, 1.5 )
    , ( C, 0.5 )
    , ( D, 0 )
    ]


balancedShares : List ( Rating, Float )
balancedShares =
    [ ( AAAAAA, 2 )
    , ( AAAAA, 6 )
    , ( AAAA, 14 )
    , ( AAA, 16 )
    , ( AAE, 18 )
    , ( AA, 15 )
    , ( AE, 12 )
    , ( A, 9 )
    , ( B, 5 )
    , ( C, 2 )
    , ( D, 1 )
    ]


progressiveShares : List ( Rating, Float )
progressiveShares =
    [ ( AAAAAA, 1 )
    , ( AAAAA, 2 )
    , ( AAAA, 7 )
    , ( AAA, 10 )
    , ( AAE, 14 )
    , ( AA, 15 )
    , ( AE, 17 )
    , ( A, 15 )
    , ( B, 10 )
    , ( C, 6 )
    , ( D, 3 )
    ]
