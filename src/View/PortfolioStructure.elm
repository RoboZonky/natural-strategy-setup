module View.PortfolioStructure exposing (defaultPortfolioForm, portfolioSharesForm)

import AllDict as Dict
import Data.Portfolio as Portfolio exposing (Portfolio(..))
import Data.PortfolioShare exposing (PortfolioShare, PortfolioShares)
import Data.Rating as Rating
import Html exposing (Html, caption, div, h2, input, option, select, table, td, text, th, tr)
import Html.Attributes as Attr exposing (size, style, type_, value)
import Html.Events exposing (onInput)
import Types exposing (..)


defaultPortfolioForm : Html Msg
defaultPortfolioForm =
    div [] [ text "Robot má udržovat ", defaultPortfolioSelect, text " portfolio." ]


defaultPortfolioSelect : Html Msg
defaultPortfolioSelect =
    select [ onInput (PortfolioChanged << Portfolio.fromString) ] <|
        List.map
            (\portfolio ->
                option
                    [ value (toString portfolio) ]
                    [ text (Portfolio.toString portfolio) ]
            )
            [ Conservative, Balanced, Progressive, Empty ]


portfolioSharesForm : Portfolio -> PortfolioShares -> Html Msg
portfolioSharesForm portfolio shares =
    div []
        [ h2 [] [ text "Struktura portfolia" ]
        , defaultPortfolioForm
        , ratingSharesTable portfolio shares
        ]


ratingSharesTable : Portfolio -> PortfolioShares -> Html Msg
ratingSharesTable portfolio shares =
    let
        ( rowRenderingFunction, tableDescription ) =
            case portfolio of
                Empty ->
                    ( portfolioShareEditableRow, "Nastavte" )

                _ ->
                    ( portfolioShareReadOnlyRow, "Následující tabulka ukazuje" )

        headerRow =
            tr []
                [ th [] [ text "Rating" ]
                , th [] [ text "minimálně" ]
                , th [] [ text "maximálně" ]
                ]

        dataRows =
            List.map rowRenderingFunction <| Dict.toList shares

        sumOfShareMinimums =
            Dict.foldr (\_ ( min, _ ) acc -> min + acc) 0 shares

        validationErrors =
            if sumOfShareMinimums /= 100 then
                [ div [ style [ ( "color", "red" ) ] ]
                    [ text <| "Součet minim musí být přesně 100% (teď je " ++ toString sumOfShareMinimums ++ "%)" ]
                ]
            else
                []
    in
    div [] <|
        [ text <| tableDescription ++ " požadovaný podíl aktuální zůstatkové částky investovaný do půjček v daném ratingu (v %)"
        , table [] (headerRow :: dataRows)
        ]
            ++ validationErrors


portfolioShareReadOnlyRow : PortfolioShare -> Html Msg
portfolioShareReadOnlyRow ( rtg, ( mi, mx ) ) =
    tr []
        [ td [] [ text <| Rating.ratingToString rtg ]
        , td [] [ text <| toString mi ]
        , td [] [ text <| toString mx ]
        ]


portfolioShareEditableRow : PortfolioShare -> Html Msg
portfolioShareEditableRow ( rtg, ( mi, mx ) ) =
    let
        inputCell val msg =
            input [ size 2, type_ "number", Attr.min "0", Attr.max "100", value (toString val), onInput msg ] []

        validationError =
            if mi > mx then
                [ td [ style [ ( "color", "red" ), ( "border", "none" ) ] ]
                    [ text "Minimum musí být menší nebo rovno než maximum" ]
                ]
            else
                []
    in
    tr [] <|
        [ td [] [ text <| Rating.ratingToString rtg ]
        , td [] [ inputCell mi (ChangePortfolioShareMin rtg) ]
        , td [] [ inputCell mx (ChangePortfolioShareMax rtg) ]
        ]
            ++ validationError
