module View.Confirmation exposing (form)

import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Data.Confirmation exposing (ConfirmationSettings)
import Data.Filter.Conditions.Rating as Rating
import Html exposing (legend, text)
import Types exposing (Msg(ConfirmationFormMsg))


form : ConfirmationSettings -> Card.BlockItem Msg
form settings =
    Card.custom <|
        Form.group []
            [ legend [] [ text "Potvrzovat mobilem investice do úvěrů s ratingem" ]
            , Html.map ConfirmationFormMsg <| Rating.form settings
            ]
