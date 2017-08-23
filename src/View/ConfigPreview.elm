module View.ConfigPreview exposing (view)

import Base64
import Data.Strategy as Strategy exposing (StrategyConfiguration)
import Html exposing (Html, a, div, text, textarea)
import Html.Attributes exposing (cols, downloadAs, href, readonly, rows, style, value, width)
import Types exposing (..)


view : StrategyConfiguration -> Html Msg
view model =
    let
        strategyString =
            Strategy.renderStrategyConfiguration model
    in
    div [ style [ ( "position", "fixed" ), ( "right", "0" ), ( "top", "0" ), ( "width", "50%" ) ] ]
        [ textarea
            [ rows 50
            , style [ ( "width", "100%" ) ]
            , value strategyString
            , readonly True
            ]
            []
        , div []
            [ a
                [ href <| "data:text/plain;charset=utf-8;base64," ++ Base64.encode strategyString
                , downloadAs <| "strategy.txt"
                ]
                [ text "Stáhnout konfigurační soubor" ]
            ]
        ]
