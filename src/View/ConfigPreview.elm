module View.ConfigPreview exposing (view)

import Base64
import Bootstrap.Button as Button
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Data.Strategy as Strategy exposing (StrategyConfiguration)
import Html exposing (text)
import Html.Attributes exposing (downloadAs, href, readonly, style)
import Types exposing (..)


view : StrategyConfiguration -> Grid.Column Msg
view model =
    let
        strategyString =
            Strategy.renderStrategyConfiguration model

        strategyStringRowCount =
            List.length <| String.lines strategyString
    in
    Grid.col
        [ Col.xs6 ]
        [ Grid.row []
            [ Grid.col []
                [ Textarea.textarea
                    [ Textarea.rows <| strategyStringRowCount + 1
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
