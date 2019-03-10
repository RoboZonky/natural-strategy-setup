module Data.Filter.Conditions.Rating exposing
    ( Rating(..)
    , RatingCondition(..)
    , allRatings
    , conditionDecoder
    , defaultCondition
    , encodeCondition
    , fromHash
    , hash
    , initRatingDict
    , showInterest
    , showInterestPercent
    , toInterestPercent
    )

import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util


type Rating
    = AAAAA
    | AAAA
    | AAA
    | AA
    | A
    | B
    | C
    | D


allRatings : List Rating
allRatings =
    [ AAAAA
    , AAAA
    , AAA
    , AA
    , A
    , B
    , C
    , D
    ]


toInterestPercent : Rating -> Float
toInterestPercent r =
    case r of
        AAAAA ->
            3.99

        AAAA ->
            4.99

        AAA ->
            5.99

        AA ->
            8.49

        A ->
            10.99

        B ->
            13.49

        C ->
            15.49

        D ->
            19.99


showInterest : Rating -> String
showInterest =
    Util.formatPercentage << toInterestPercent


showInterestPercent : Rating -> String
showInterestPercent r =
    showInterest r ++ " % p.a."


initRatingDict : List ( Rating, a ) -> AnyDict Int Rating a
initRatingDict =
    Dict.Any.fromList hash


hash : Rating -> Int
hash rating =
    case rating of
        AAAAA ->
            1

        AAAA ->
            2

        AAA ->
            3

        AA ->
            4

        A ->
            5

        B ->
            6

        C ->
            7

        D ->
            8


fromHash : Int -> Maybe Rating
fromHash h =
    case h of
        1 ->
            Just AAAAA

        2 ->
            Just AAAA

        3 ->
            Just AAA

        4 ->
            Just AA

        5 ->
            Just A

        6 ->
            Just B

        7 ->
            Just C

        8 ->
            Just D

        _ ->
            Nothing


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



-- The stuff below is kept just to implement migration from URL-persisted V1 strategies


defaultCondition : RatingCondition
defaultCondition =
    RatingList []
