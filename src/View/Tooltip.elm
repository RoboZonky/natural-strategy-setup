module View.Tooltip exposing (popoverTip, popoverTipForModal)

import Bootstrap.Popover as Popover
import Data.Tooltip as Tooltip exposing (TipId)
import Html exposing (Html, div, i, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (defaultOptions, onWithOptions)
import Json.Decode
import Types exposing (ModalMsg(ModalNoOp, ModalTooltipMsg), Msg(NoOp, TooltipMsg))


icon : Int -> Html a
icon width =
    i
        [ class "fa fa-question-circle ml-1"
        , style [ ( "font-size", "18px" ), ( "color", "gray" ) ]
        ]
        []


popoverTip : TipId -> Tooltip.States -> Html Msg
popoverTip =
    popover TooltipMsg NoOp (icon 18)


popoverTipForModal : TipId -> Tooltip.States -> Html ModalMsg
popoverTipForModal =
    popover ModalTooltipMsg ModalNoOp (icon 18)


{-| Tip in a rectangle that is hidden by default and is displayed by hovering mouse over elementToHoverOn
-}
popover : (TipId -> Popover.State -> msg) -> msg -> Html msg -> TipId -> Tooltip.States -> Html msg
popover tipMsg noOpMsg elementToHoverOn tipId tooltipStates =
    let
        popoverState =
            Tooltip.getState tipId tooltipStates

        popoverText =
            Tooltip.getTooltipText tipId
    in
    Popover.config (div (Popover.onHover popoverState (tipMsg tipId) ++ [ onClickNoOp noOpMsg ]) [ elementToHoverOn ])
        |> Popover.right
        |> Popover.content [] [ text popoverText ]
        |> Popover.view popoverState



{- Prevent clicks on tooltip icon from being propagated to the background. This was causing problems when tooltip icon was in the accordion header -}


onClickNoOp : msg -> Html.Attribute msg
onClickNoOp noOpMsg =
    onWithOptions "click" { defaultOptions | stopPropagation = True } (Json.Decode.succeed noOpMsg)