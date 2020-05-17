module View.Tooltip exposing
    ( Config
    , popoverTip
    )

import Bootstrap.Popover as Popover
import Bootstrap.Utilities.Spacing as Spacing
import Data.Tooltip as Tooltip exposing (TipId)
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events exposing (stopPropagationOn)
import Json.Decode


icon : Html a
icon =
    Html.i
        [ class "fa fa-question-circle"
        , style "font-size" "18px"
        , style "color" "gray"
        , Spacing.ml1
        ]
        []


type alias Config msg =
    { tooltipMsg : TipId -> Popover.State -> msg
    , noOp : msg
    }


popoverTip : Config msg -> TipId -> Tooltip.States -> Html msg
popoverTip { tooltipMsg, noOp } =
    popover tooltipMsg noOp icon


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
    Popover.config (Html.div (Popover.onHover popoverState (tipMsg tipId) ++ [ onClickNoOp noOpMsg ]) [ elementToHoverOn ])
        |> Popover.right
        |> Popover.content [] [ Html.text popoverText ]
        |> Popover.view popoverState


{-| Prevent clicks on tooltip icon from being propagated to the background.
This was causing problems when tooltip icon was in the accordion header
-}
onClickNoOp : msg -> Html.Attribute msg
onClickNoOp noOpMsg =
    stopPropagationOn "click" <| Json.Decode.succeed ( noOpMsg, True )
