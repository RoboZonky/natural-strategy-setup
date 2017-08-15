module View.ConfigPreview exposing (view)

import Data.Strategy as Strategy
import Html exposing (Html, textarea)
import Html.Attributes exposing (cols, readonly, rows, style, value)
import Types exposing (..)


view : Model -> Html Msg
view model =
    textarea
        [ rows 50
        , cols 80
        , value (Strategy.renderParsedStrategy model)
        , readonly True
        , style [ ( "position", "fixed" ), ( "right", "0" ), ( "top", "0" ), ( "width", "50%" ) ]
        ]
        []
