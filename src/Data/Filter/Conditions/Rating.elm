module Data.Filter.Conditions.Rating exposing
    ( Rating(..)
    , RatingCondition(..)
    , allRatings
    , conditionDecoder
    , encodeCondition
    , initRatingDict
    , showInterestPercent
    , toInterestPercent
    )

import Dict.Any exposing (AnyDict)
import FormatNumber
import FormatNumber.Locales exposing (Locale)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util


type Rating
    = A_Double_Star
    | A_Star
    | A_Double_Plus
    | A_Plus
    | A
    | B
    | C
    | D


allRatings : List Rating
allRatings =
    [ A_Double_Star
    , A_Star
    , A_Double_Plus
    , A_Plus
    , A
    , B
    , C
    , D
    ]


formatPercentage : Float -> String
formatPercentage =
    FormatNumber.format czechLocale


czechLocale : Locale
czechLocale =
    { decimals = 2
    , thousandSeparator = ""
    , decimalSeparator = ","
    , negativePrefix = "−"
    , negativeSuffix = ""
    , positivePrefix = ""
    , positiveSuffix = ""
    }


toInterestPercent : Rating -> Float
toInterestPercent r =
    case r of
        A_Double_Star ->
            3.99

        A_Star ->
            4.99

        A_Double_Plus ->
            5.99

        A_Plus ->
            8.49

        A ->
            10.99

        B ->
            13.49

        C ->
            15.49

        D ->
            19.99


showInterestPercent : Rating -> String
showInterestPercent r =
    formatPercentage (toInterestPercent r) ++ " % p.a."


initRatingDict : List ( Rating, a ) -> AnyDict Int Rating a
initRatingDict =
    Dict.Any.fromList hash


hash : Rating -> Int
hash rating =
    case rating of
        A_Double_Star ->
            1

        A_Star ->
            2

        A_Double_Plus ->
            3

        A_Plus ->
            4

        A ->
            5

        B ->
            6

        C ->
            7

        D ->
            8


type RatingCondition
    = RatingList (List Rating)



-- JSON


encodeRating : Rating -> Value
encodeRating =
    Util.enumEncoder allRatings


encodeCondition : RatingCondition -> Value
encodeCondition (RatingList rs) =
    Encode.list encodeRating rs


ratingDecoder : Decoder Rating
ratingDecoder =
    Util.enumDecoder "Rating" allRatings


conditionDecoder : Decoder RatingCondition
conditionDecoder =
    Decode.map RatingList <|
        Decode.list ratingDecoder
