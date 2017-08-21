module View.Confirmation exposing (form)

import Data.Confirmation as Confirmation exposing (ConfirmationSettings)
import Data.Rating exposing (RatingCondition(RatingList))
import Html exposing (Html, fieldset, legend, p, text)
import Types exposing (..)
import View.RatingCondition as RatingCondition


form : ConfirmationSettings -> Html Msg
form settings =
    fieldset []
        [ legend [] [ text "Potvrzovat mobilem investice do úvěrů s ratingem " ]
        , RatingCondition.ratingCheckboxes (RatingList <| Confirmation.getRatingsWithEnabledConfirmation settings)
        ]
