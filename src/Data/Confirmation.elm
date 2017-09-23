module Data.Confirmation
    exposing
        ( ConfirmationSettings
        , confirmationsDisabled
        , renderConfirmation
        )

import Data.Filter.Conditions.Rating as Rating exposing (RatingCondition(..))


type alias ConfirmationSettings =
    RatingCondition


confirmationsDisabled : ConfirmationSettings
confirmationsDisabled =
    RatingList []


renderConfirmation : ConfirmationSettings -> String
renderConfirmation (RatingList enabledRatings) =
    if List.isEmpty enabledRatings then
        ""
    else
        "Potvrzovat mobilem investice do úvěrů, kde " ++ Rating.renderRatingCondition (RatingList enabledRatings) ++ "."
