module Data.Rating
    exposing
        ( Rating(..)
        , RatingCondition(..)
        , hash
        , ratingToString
        , renderRatingCondition
        )

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
            0

        A_Star ->
            1

        A_Double_Plus ->
            2

        A_Plus ->
            3

        A ->
            4

        B ->
            5

        C ->
            6

        D ->
            7


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
