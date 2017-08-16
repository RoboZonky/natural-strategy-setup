module Data.Strategy.Rating
    exposing
        ( Rating(..)
        , RatingCondition(..)
        , renderRatingCondition
        , ratingToString
        )


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
renderRatingList rs =
    case rs of
        [] ->
            "VYBERTE ASPOŇ JEDNU HODNOTU"

        r :: [] ->
            ratingToString r

        r1 :: r2 :: [] ->
            ratingToString r1 ++ " nebo " ++ ratingToString r2

        r1 :: rest ->
            ratingToString r1 ++ ", " ++ renderRatingList rest
