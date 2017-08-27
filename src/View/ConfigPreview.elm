module View.ConfigPreview exposing (view)

import Base64
import Bootstrap.Button as Button
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Data.Strategy as Strategy exposing (StrategyConfiguration)
import Html exposing (Html, a, div, text, textarea)
import Html.Attributes exposing (cols, downloadAs, href, readonly, rows, style, value, width)
import Types exposing (..)


view : StrategyConfiguration -> Grid.Column Msg
view model =
    let
        strategyString =
            Strategy.renderStrategyConfiguration model
    in
    Grid.col
        [ Col.xs6 ]
        [ Grid.row []
            [ Grid.col []
                [ Textarea.textarea
                    [ Textarea.rows 40
                    , Textarea.value strategyString
                    , Textarea.attrs [ readonly True, style [ ( "width", "100%" ) ] ]
                    ]
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ Button.linkButton
                    [ Button.primary
                    , Button.attrs
                        [ href <| "data:text/plain;charset=utf-8;base64," ++ Base64.encode strategyString
                        , downloadAs <| "strategy.txt"
                        ]
                    ]
                    [ text "Stáhni konfigurační soubor" ]
                ]
            ]
        ]
