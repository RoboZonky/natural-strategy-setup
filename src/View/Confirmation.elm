module View.Confirmation exposing (form)

import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form.Fieldset as Fieldset
import Data.Confirmation exposing (ConfirmationSettings)
import Data.Filter.Conditions.Rating as Rating
import Data.Tooltip as Tooltip
import Html
import Types exposing (Msg(..))
import View.Tooltip as Tooltip


form : ConfirmationSettings -> Tooltip.States -> CardBlock.Item Msg
form settings tooltipStates =
    Fieldset.config
        |> Fieldset.asGroup
        |> Fieldset.legend []
            [ Html.text "Potvrzovat mobilem investice do úvěrů s ratingem"
            , Tooltip.popoverTip Tooltip.zonkoidTip tooltipStates
            ]
        |> Fieldset.children [ Html.map ConfirmationFormMsg (Rating.form "confirm_" settings) ]
        |> Fieldset.view
        |> CardBlock.custom
