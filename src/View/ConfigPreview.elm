module View.ConfigPreview exposing (view)

import Base64
import Bootstrap.Button as Button
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Data.Strategy as Strategy exposing (StrategyConfiguration)
import Html
import Html.Attributes exposing (download, href, readonly, style)
import Time exposing (Posix)
import Types exposing (BaseUrl)


view : BaseUrl -> Posix -> StrategyConfiguration -> Grid.Column a
view baseUrl generatedOn strategyConfig =
    let
        strategyValidationErrors =
            Strategy.validateStrategyConfiguration strategyConfig

        isValidStrategy =
            List.isEmpty strategyValidationErrors

        previewText =
            if isValidStrategy then
                Strategy.renderStrategyConfiguration baseUrl generatedOn strategyConfig

            else
                String.join "\n" <|
                    "Konfigurace nemůže být zobrazena, protože formulář obsahuje chyby:"
                        :: strategyValidationErrors

        previewTextRowCount =
            List.length <| String.lines previewText
    in
    Grid.col
        [ Col.xs6 ]
        [ Grid.row []
            [ Grid.col []
                [ Textarea.textarea
                    [ Textarea.rows <| previewTextRowCount + extraRowsForStrategyUrl
                    , Textarea.value previewText
                    , Textarea.attrs
                        [ readonly True
                        , style "width" "100%"
                        , style "height" "80vh"
                        ]
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
                        , download "robozonky-strategy.cfg"
                        ]
                    ]
                    [ Html.text "Stáhni konfigurační soubor" ]
                ]
            ]
        ]


{-| Heuristically determined number of rows to automatically expand text area
by, to avoid unnecessary scrollbar. The previous heuristic (1 text area row per
1line of strategy config) no longer works now that we have shareable URLs,
because the URL consists of just one line but is nevertheless rendered over
multiple lines. The extraRows value is based on rough estimate of average
shareable url length (500 for moderately complex strategies) and normal screen
size (about 1k px).
-}
extraRowsForStrategyUrl : Int
extraRowsForStrategyUrl =
    6
