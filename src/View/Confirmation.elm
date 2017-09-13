module View.Confirmation exposing (form)

import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Data.Confirmation as Confirmation exposing (ConfirmationSettings)
import Data.Filter.Condition.Rating as Rating exposing (RatingCondition(RatingList))
import Data.Tooltip as Tooltip
import Html exposing (Html, fieldset, legend, p, text)
import Types exposing (..)
import View.Tooltip as Tooltip


form : ConfirmationSettings -> Tooltip.States -> Card.BlockItem Msg
form settings tooltipStates =
    Card.custom <|
        Form.group []
            [ legend []
                [ text "Potvrzovat mobilem investice do úvěrů s ratingem "
                , Tooltip.popoverTip Tooltip.confirmationTip tooltipStates
                ]
            , Html.map ConfirmationFormMsg <| Rating.ratingForm settings
            ]
