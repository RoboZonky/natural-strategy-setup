module View.Confirmation exposing (form)

import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Radio as Radio
import Bootstrap.Utilities.Spacing as Spacing
import Data.Confirmation as Confirmation exposing (ConfirmationFormMsg(..), ConfirmationSettings(..))
import Data.Filter.Conditions.Interest as Interest
import Data.Tooltip as Tooltip
import Html
import Types exposing (Msg(..))
import Util
import View.Tooltip as Tooltip


form : ConfirmationSettings -> Tooltip.States -> CardBlock.Item Msg
form confirmationSettings tooltipStates =
    let
        ( confirmationEnabled, subform ) =
            case confirmationSettings of
                NoConfirmation ->
                    ( False, Html.text "" )

                Confirm interestCondition ->
                    ( True
                    , Html.div [ Spacing.mx5 ]
                        [ Html.map (ConfirmationFormMsg << UpdateConfirmation)
                            (Interest.form interestCondition)
                        ]
                    )

        validationErrors =
            Util.viewErrors <| Confirmation.validate confirmationSettings
    in
    Fieldset.config
        |> Fieldset.asGroup
        |> Fieldset.legend []
            [ Html.text "Potvrzení investic mobilem"
            , Tooltip.popoverTip Tooltip.zonkoidTip tooltipStates
            ]
        |> Fieldset.children
            [ Radio.radio
                [ Radio.id "confirmOff"
                , Radio.checked (not confirmationEnabled)
                , radioName
                , Radio.onClick (ConfirmationFormMsg DisableConfirmation)
                ]
                "Investovat bez potvrzení"
            , Radio.radio
                [ Radio.id "confirmOn"
                , Radio.checked confirmationEnabled
                , radioName
                , Radio.onClick (ConfirmationFormMsg EnableConfirmation)
                ]
                "Potvrzovat mobilem investice do úvěrů kde úrok"
            , subform
            , validationErrors
            ]
        |> Fieldset.view
        |> CardBlock.custom


radioName : Radio.Option msg
radioName =
    Radio.name "mobileConfirmations"
