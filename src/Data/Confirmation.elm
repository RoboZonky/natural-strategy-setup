module Data.Confirmation
    exposing
        ( ConfirmationSettings
        , confirmationsDisabled
        , getRatingsWithEnabledConfirmation
        , renderConfirmation
        )

import AllDict exposing (AllDict)
import Data.Rating as Rating exposing (Rating, RatingCondition)


type alias ConfirmationSettings =
    AllDict Rating Bool Int


confirmationsDisabled : ConfirmationSettings
confirmationsDisabled =
    AllDict.empty Rating.hash


renderConfirmation : ConfirmationSettings -> String
renderConfirmation settings =
    let
        enabledRatings =
            getRatingsWithEnabledConfirmation settings

        condition =
            Rating.determineRatingCondition enabledRatings
    in
    if List.isEmpty enabledRatings then
        ""
    else
        "Potvrzovat mobilem investice do úvěrů, kde " ++ Rating.renderRatingCondition condition ++ "."


getRatingsWithEnabledConfirmation : ConfirmationSettings -> List Rating
getRatingsWithEnabledConfirmation settings =
    AllDict.toList settings
        |> List.filterMap
            (\( rating, isRatingEnabled ) ->
                if isRatingEnabled then
                    Just rating
                else
                    Nothing
            )
