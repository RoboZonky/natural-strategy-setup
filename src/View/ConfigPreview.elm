module View.ConfigPreview exposing (view)

import Data.Strategy as Strategy exposing (ParsedStrategy)
import Html exposing (Html, a, div, text, textarea)
import Html.Attributes exposing (cols, downloadAs, href, readonly, rows, style, value, width)
import Types exposing (..)


view : ParsedStrategy -> Html Msg
view model =
    let
        strategyString =
            Strategy.renderParsedStrategy model
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
                [ text "Download "
                , a
                    [ href <| "data:text/plain;charset=utf-8," ++ strategyString
                    , downloadAs <| "strategy.txt"
                    ]
                    [ text "strategy file" ]
                ]
            ]
