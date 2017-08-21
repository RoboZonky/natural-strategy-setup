module Data.Rating
    exposing
        ( Rating(..)
        , RatingCondition(..)
        , determineRatingCondition
        , hash
        , ratingToString
        , renderRatingCondition
        , ratingSatisfiesCondition
        , allRatings
        )

import List.Extra as List
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
    [ A_Double_Star, A_Star, A_Double_Plus, A_Plus, A, B, C, D ]


ratingToString : Rating -> String
ratingToString r =
    case r of
        A_Double_Star ->
            "A**"

        A_Star ->
            "A*"

        A_Double_Plus ->
            "A++"

        A_Plus ->
            "A+"

        A ->
            "A"

        B ->
            "B"

        C ->
            "C"

        D ->
            "D"



-- TODO Elm 0.18 doesn't make it possible to use Union type values as keys in Dict
-- As a workaround using eeue56/elm-all-dict which makes that possible, but requires explicit key hashing function
-- Expecting this to be rectified in 0.19


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
    | BetterThan Rating
    | WorseThan Rating


renderRatingCondition : RatingCondition -> String
renderRatingCondition ratingCondition =
    let
        subExpr =
            case ratingCondition of
                RatingList rs ->
                    renderRatingList rs

                BetterThan r ->
                    "lepší než " ++ ratingToString r

                WorseThan r ->
                    "horší než " ++ ratingToString r
    in
        "rating je " ++ subExpr


renderRatingList : List Rating -> String
renderRatingList =
    Util.orList ratingToString


determineRatingCondition : List Rating -> RatingCondition
determineRatingCondition ratings =
    let
        sortedHashes =
            List.sort <| List.map hash ratings

        len =
            List.length ratings

        ratingCount =
            8

        allHashes =
            List.range 1 ratingCount
    in
        if List.isPrefixOf sortedHashes allHashes && 0 < len && len < ratingCount then
            BetterThan <| Maybe.withDefault A_Double_Star <| List.head <| List.drop len allRatings
        else if List.isSuffixOf sortedHashes allHashes && 0 < len && len < ratingCount then
            WorseThan <| Maybe.withDefault D <| List.last <| List.take (ratingCount - len) allRatings
        else
            RatingList ratings


ratingSatisfiesCondition : RatingCondition -> Rating -> Bool
ratingSatisfiesCondition condition rating =
    case condition of
        RatingList list ->
            List.member rating list

        BetterThan referenceRating ->
            hash rating < hash referenceRating

        WorseThan referenceRating ->
            hash referenceRating < hash rating
