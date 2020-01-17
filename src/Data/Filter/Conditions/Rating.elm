module Data.Filter.Conditions.Rating exposing
    ( Rating(..)
    , RatingCondition(..)
    , allRatings
    , conditionDecoder
    , defaultCondition
    , fromHash
    , hash
    , initRatingDict
    , ratingDictToList
    , showInterest
    , showInterestPercent
    , toColorClass
    , toInterestPercent
    )

import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import Util


type Rating
    = AAAAAA
    | AAAAA
    | AAAA
    | AAA
    | AAE
    | AA
    | AE
    | A
    | B
    | C
    | D


allRatings : List Rating
allRatings =
    [ AAAAAA
    , AAAAA
    , AAAA
    , AAA
    , AAE
    , AA
    , AE
    , A
    , B
    , C
    , D
    ]


toInterestPercent : Rating -> Float
toInterestPercent r =
    case r of
        AAAAAA ->
            2.99

        AAAAA ->
            3.99

        AAAA ->
            4.99

        AAA ->
            5.99

        AAE ->
            6.99

        AA ->
            8.49

        AE ->
            9.49

        A ->
            10.99

        B ->
            13.49

        C ->
            15.49

        D ->
            19.99


{-| This is to associate colors with slider thumbs.
Ideally I'd want to set color programmatically, but didn't figure out how to do it,
because it's associated with elements via CSS pseudo classes, like: "-moz-range-thumb"
-}
toColorClass : Rating -> String
toColorClass rating =
    "slider-color-"
        ++ (case rating of
                AAAAAA ->
                    "aaaaaa"

                AAAAA ->
                    "aaaaa"

                AAAA ->
                    "aaaa"

                AAA ->
                    "aaa"

                AAE ->
                    "aae"

                AA ->
                    "aa"

                AE ->
                    "ae"

                A ->
                    "a"

                B ->
                    "b"

                C ->
                    "c"

                D ->
                    "d"
           )


showInterest : Rating -> String
showInterest =
    Util.formatPercentage << toInterestPercent


showInterestPercent : Rating -> String
showInterestPercent r =
    showInterest r ++ " % p.a."


initRatingDict : List ( Rating, a ) -> AnyDict Int Rating a
initRatingDict =
    Dict.Any.fromList hash


{-| Result sorted by interest percentage corresponding to each rating from lowest (AAAAA first) to largest (D)
-}
ratingDictToList : AnyDict Int Rating a -> List ( Rating, a )
ratingDictToList =
    Dict.Any.toList >> List.sortBy (Tuple.first >> toInterestPercent)


hash : Rating -> Int
hash rating =
    case rating of
        AAAAAA ->
            11

        AAAAA ->
            1

        AAAA ->
            2

        AAA ->
            3

        AAE ->
            9

        AA ->
            4

        AE ->
            10

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
        11 ->
            Just AAAAAA

        1 ->
            Just AAAAA

        2 ->
            Just AAAA

        3 ->
            Just AAA

        9 ->
            Just AAE

        4 ->
            Just AA

        10 ->
            Just AE

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
