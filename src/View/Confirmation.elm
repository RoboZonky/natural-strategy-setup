module View.Confirmation exposing (form)

import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Data.Confirmation exposing (ConfirmationSettings)
import Data.Filter.Conditions.Rating as Rating
import Data.Tooltip as Tooltip
import Html exposing (legend, text)
import Types exposing (Msg(ConfirmationFormMsg))
import View.Tooltip as Tooltip


form : ConfirmationSettings -> Tooltip.States -> Card.BlockItem Msg
form settings tooltipStates =
    Card.custom <|
        Form.group []
            [ legend []
                [ text "Potvrzovat mobilem investice do úvěrů s ratingem"
                , Tooltip.popoverTip Tooltip.confirmationTip tooltipStates
                ]
            , Html.map ConfirmationFormMsg <| Rating.form settings
            ]
