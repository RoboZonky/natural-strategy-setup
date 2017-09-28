module View.ConfigPreview exposing (view)

import Base64
import Bootstrap.Button as Button
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Data.Strategy as Strategy exposing (StrategyConfiguration)
import Html exposing (text)
import Html.Attributes exposing (downloadAs, href, readonly, style)
import Time.DateTime exposing (DateTime)
import Types exposing (..)


view : DateTime -> StrategyConfiguration -> Grid.Column Msg
view generatedOn strategyConfig =
    let
        strategyValidationErrors =
            Strategy.validateStrategyConfiguration strategyConfig

        isValidStrategy =
            List.isEmpty strategyValidationErrors

        previewText =
            if isValidStrategy then
                Strategy.renderStrategyConfiguration generatedOn strategyConfig
            else
                String.join "\n" <| "Konfigurace nemůže být zobrazena, protože formulář obsahuje chyby:" :: strategyValidationErrors

        previewTextRowCount =
            List.length <| String.lines previewText
    in
    Grid.col
        [ Col.xs6 ]
        [ Grid.row []
            [ Grid.col []
                [ Textarea.textarea
                    [ Textarea.rows <| previewTextRowCount + 1
                    , Textarea.value previewText
                    , Textarea.attrs [ readonly True, style [ ( "width", "100%" ) ] ]
                    ]
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ Button.linkButton
                    [ Button.primary
                    , Button.disabled <| not isValidStrategy
                    , Button.attrs
                        [ href <| "data:text/plain;charset=utf-8;base64," ++ Base64.encode previewText
                        , downloadAs <| "strategy.txt"
                        ]
                    ]
                    [ text "Stáhni konfigurační soubor" ]
                ]
            ]
        ]
