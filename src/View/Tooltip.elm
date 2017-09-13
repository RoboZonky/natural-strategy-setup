module View.Tooltip exposing (popoverTip, popoverTipForModal)

import Bootstrap.Popover as Popover
import Data.Tooltip as Tooltip exposing (TipId)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (defaultOptions, onWithOptions)
import Json.Decode
import Svg exposing (Svg)
import Svg.Attributes exposing (d, fill, viewBox)
import Types exposing (ModalMsg(ModalNoOp, ModalTooltipMsg), Msg(NoOp, TooltipMsg))


icon : Int -> Html a
icon width =
    Svg.svg [ viewBox "0 0 1000 1000", style [ ( "width", toString width ++ "px" ) ] ]
        [ Svg.path [ d "M500.1,9.9C229.4,9.9,10,229.1,10,499.8c0,270.7,219.4,490.3,490.1,490.3S990,770.5,990,499.8C990,229.1,770.7,9.9,500.1,9.9z M500.3,879.2c-209.5,0-379.6-169.9-379.6-379.5c0-209.4,170-379,379.6-379c209.2,0,379,169.6,379,379C879.2,709.4,709.5,879.2,500.3,879.2z", fill "grey" ] []
        , Svg.path [ d "M457.7,645.5h93v-72.7c0-19.6,9.2-38,33.8-54.2c24.3-16.1,92.7-48.6,92.7-134.1c0-85.7-71.8-144.7-132-157.2c-60.5-12.5-125.9-4.3-172.1,46.5c-41.6,45.3-50.3,81.5-50.3,160.9h93v-18.6c0-42.1,4.9-86.9,65.4-99.1c33-6.7,64,3.8,82.3,21.6c21.1,20.6,21.1,66.7-12.3,89.9l-52.5,35.5c-30.6,19.8-40.9,41.6-40.9,73.7V645.5L457.7,645.5z", fill "grey" ] []
        , Svg.path [ d "M504.3,681.9c26.3,0,47.8,21.4,47.8,47.9c0,26.5-21.5,47.8-47.8,47.8c-26.6,0-48.3-21.4-48.3-47.8C456.1,703.3,477.7,681.9,504.3,681.9z", fill "grey" ] []
        ]


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
