module View.NumericInput exposing (numericInput)

import Bootstrap.Form.Input as Input
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (Html)
import Html.Attributes as Attr


numericInput : Int -> Int -> (String -> msg) -> Bool -> String -> Html msg
numericInput minValue maxValue toMessage enabled value =
    Input.number
        [ Input.small
        , Input.onInput toMessage
        , Input.disabled <| not enabled
        , Input.value value
        , Input.attrs
            [ Attr.min <| toString minValue
            , Attr.max <| toString maxValue
            , Spacing.mx1
            ]
        ]
