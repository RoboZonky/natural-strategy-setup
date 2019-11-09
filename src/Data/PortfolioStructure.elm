module Data.PortfolioStructure exposing
    ( PortfolioShare
    , PortfolioShares
    , balanced
    , conservative
    , decoder
    , decoderFromPortfolio
    , encode
    , fromPercentageList
    , portfolioSharesEqual
    , progressive
    , renderPortfolioShares
    , shareSum
    , validate
    )

import Data.Filter.Conditions.Rating as Rating exposing (Rating(..), ratingDictToList)
import Data.Portfolio exposing (Portfolio(..))
import Data.Validate as Validate
import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Percentage exposing (Percentage)
import Util


type alias PortfolioShares =
    AnyDict Int Rating Share


type alias PortfolioShare =
    ( Rating, Share )



-- TODO  remove this alias


type alias Share =
    Percentage


renderPortfolioShare : PortfolioShare -> String
renderPortfolioShare ( rating, share ) =
    "Prostředky úročené "
        ++ Rating.showInterestPercent rating
        ++ " mají tvořit "
        ++ renderShare share
        ++ " % aktuální zůstatkové částky."


renderShare : Share -> String
renderShare =
    String.fromInt << Percentage.toInt


renderPortfolioShares : Portfolio -> PortfolioShares -> String
renderPortfolioShares portfolio shares =
    case portfolio of
        UserDefined ->
            Dict.Any.toList shares
                |> List.sortBy (\( rating, _ ) -> Rating.toInterestPercent rating)
                -- Only render share in the config when maximum > 0
                |> List.filter (\( _, percentage ) -> Percentage.toInt percentage > 0)
                |> List.map renderPortfolioShare
                |> Util.renderNonemptySection "\n- Úprava struktury portfolia"

        _ ->
            ""


shareSum : PortfolioShares -> Int
shareSum =
    Dict.Any.foldr (\_ percentage sumAcc -> sumAcc + Percentage.toInt percentage) 0


validate : PortfolioShares -> List String
validate portfolioShares =
    let
        sumOfShares =
            shareSum portfolioShares
    in
    Validate.validate (sumOfShares < 100) <|
        "Součet podílů nesmí být menší než 100% (teď je "
            ++ String.fromInt sumOfShares
            ++ "%)"


portfolioSharesEqual : PortfolioShares -> PortfolioShares -> Bool
portfolioSharesEqual ps1 ps2 =
    Dict.Any.values ps1 == Dict.Any.values ps2



-- JSON


encode : PortfolioShares -> Value
encode shares =
    ratingDictToList shares
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


decoder : Decoder PortfolioShares
decoder =
    Decode.list shareDecoder
        |> Decode.map fromPercentageList
        |> Decode.andThen
            (\res ->
                case res of
                    Ok shares ->
                        Decode.succeed shares

                    Err e ->
                        Decode.fail e
            )


fromPercentageList : List Percentage -> Result String PortfolioShares
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


encodePercentage : Share -> Value
encodePercentage =
    Percentage.toInt >> Encode.int


shareDecoder : Decoder Share
shareDecoder =
    Decode.map Percentage.fromInt Decode.int


decoderFromPortfolio : Portfolio -> Decoder PortfolioShares
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


conservative : PortfolioShares
conservative =
    initShares
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


balanced : PortfolioShares
balanced =
    initShares
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


progressive : PortfolioShares
progressive =
    initShares
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


initShares : List ( Rating, Float ) -> PortfolioShares
initShares =
    List.map (Tuple.mapSecond (Basics.round >> Percentage.fromInt))
        >> Rating.initRatingDict
