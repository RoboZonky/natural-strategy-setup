module Data.Migration.Strategy.V4.PortfolioStructure exposing
    ( PortfolioShare
    , PortfolioShares
    , balanced
    , conservative
    , decoder
    , decoderFromPortfolio
    , encode
    , percentageShare
    , portfolioSharesEqual
    , progressive
    , renderPortfolioShares
    , toIntRange
    , validate
    )

import Data.Filter.Conditions.Rating as Rating exposing (Rating(..))
import Data.Portfolio exposing (Portfolio(..))
import Data.SharedJsonStuff
import Data.Validate as Validate
import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import RangeSlider exposing (RangeSlider, setDimensions, setExtents, setFormatter, setStepSize, setValues)
import Util


type alias PortfolioShares =
    AnyDict Int Rating Share


type alias PortfolioShare =
    ( Rating, Share )


type alias Share =
    RangeSlider


renderPortfolioShare : PortfolioShare -> String
renderPortfolioShare ( rating, share ) =
    "Prostředky úročené "
        ++ Rating.showInterestPercent rating
        ++ " mají tvořit "
        ++ renderShare share
        ++ " % aktuální zůstatkové částky."


renderShare : Share -> String
renderShare share =
    let
        ( minPercent, maxPercent ) =
            toIntRange share
    in
    if minPercent == maxPercent then
        String.fromInt minPercent

    else
        String.fromInt minPercent ++ " až " ++ String.fromInt maxPercent


renderPortfolioShares : Portfolio -> PortfolioShares -> String
renderPortfolioShares portfolio shares =
    case portfolio of
        UserDefined ->
            Dict.Any.toList shares
                |> List.sortBy (\( rating, _ ) -> Rating.toInterestPercent rating)
                -- Only render share in the config when maximum > 0
                |> List.filter (\( _, share ) -> Tuple.second (toIntRange share) > 0)
                |> List.map renderPortfolioShare
                |> Util.renderNonemptySection "\n- Úprava struktury portfolia"

        _ ->
            ""


toIntRange : Share -> ( Int, Int )
toIntRange =
    RangeSlider.getValues >> (\( a, b ) -> ( round a, round b ))


percentageShare : Float -> Float -> Share
percentageShare from to =
    RangeSlider.init
        |> setStepSize (Just 1.0)
        |> setFormatter (\value -> String.fromFloat value ++ "%")
        |> setDimensions 300 57
        |> setExtents 0 100
        |> setValues from to


validate : PortfolioShares -> List String
validate portfolioShares =
    let
        sumOfShareMinimums =
            Dict.Any.foldr (\_ sliderState sumAcc -> sumAcc + getSliderMinimum sliderState) 0 portfolioShares
                |> round
    in
    Validate.validate (sumOfShareMinimums /= 100) <|
        "Součet minim musí být přesně 100% (teď je "
            ++ String.fromInt sumOfShareMinimums
            ++ "%)"


getSliderMinimum : RangeSlider -> Float
getSliderMinimum =
    Tuple.first << RangeSlider.getValues


portfolioSharesEqual : PortfolioShares -> PortfolioShares -> Bool
portfolioSharesEqual ps1 ps2 =
    let
        getSliderValues =
            Dict.Any.values >> List.map RangeSlider.getValues
    in
    getSliderValues ps1 == getSliderValues ps2



-- JSON


encode : PortfolioShares -> Value
encode =
    Data.SharedJsonStuff.encodeRatingToSliderDict encodeShare


decoder : Decoder PortfolioShares
decoder =
    Data.SharedJsonStuff.ratingToSliderDictDecoder defaultShare shareDecoder


encodeShare : Share -> Value
encodeShare sz =
    toIntRange sz |> (\( from, to ) -> Encode.list Encode.int [ from, to ])


shareDecoder : Decoder Share
shareDecoder =
    Decode.list Decode.int
        |> Decode.map
            (\xs ->
                case xs of
                    from :: to :: [] ->
                        percentageShare (toFloat from) (toFloat to)

                    _ ->
                        defaultShare
            )


defaultShare : Share
defaultShare =
    percentageShare 0 0


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
    List.map (\( rtg, x ) -> ( rtg, percentageShare x x ))
        >> Rating.initRatingDict
