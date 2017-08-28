module View.PortfolioStructure exposing (defaultPortfolioForm, form)

import AllDict as Dict
import Bootstrap.Accordion as Accordion
import Bootstrap.Card as Card
import Data.Portfolio as Portfolio exposing (Portfolio(..))
import Data.PortfolioStructure exposing (PortfolioShare, PortfolioShares, Share)
import Data.Rating as Rating
import Html exposing (Html, caption, div, h2, input, option, p, select, table, td, text, th, tr)
import Html.Attributes as Attr exposing (size, style, type_, value)
import Html.Events exposing (onInput)
import Types exposing (..)


form : Portfolio -> PortfolioShares -> Accordion.Card Msg
form portfolio shares =
    Accordion.card
        { id = "portfolioStructureCard"
        , options = []
        , header = Accordion.headerH4 [] <| Accordion.toggle [] [ text "Struktura portfolia" ]
        , blocks =
            [ Accordion.block []
                [ defaultPortfolioForm
                , ratingSharesTable portfolio shares
                ]
            ]
        }


defaultPortfolioForm : Card.BlockItem Msg
defaultPortfolioForm =
    Card.custom <| div [] [ text "Robot má udržovat ", defaultPortfolioSelect, text " portfolio." ]


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


ratingSharesTable : Portfolio -> PortfolioShares -> Card.BlockItem Msg
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
                , th [] [ text "od (%)" ]
                , th [] [ text "do (%)" ]
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
    Card.custom <|
        div [] <|
            [ text <| tableDescription ++ " požadovaný procentuální podíl aktuální zůstatkové částky investovaný do půjček v daném ratingu"
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
    in
    tr [] <|
        [ td [] [ text <| Rating.ratingToString rtg ]
        , td [] [ inputCell mi (ChangePortfolioShareMin rtg) ]
        , td [] [ inputCell mx (ChangePortfolioShareMax rtg) ]
        ]
            ++ validationErrors ( mi, mx )


validationErrors : Share -> List (Html Msg)
validationErrors ( mi, mx ) =
    let
        minGtMax =
            if mi > mx then
                [ "Minimum nesmí být větší než maximum" ]
            else
                []

        maxOver100 =
            if mx > 100 then
                [ "Maximum nesmí být větší než 100" ]
            else
                []
    in
    case minGtMax ++ maxOver100 of
        [] ->
            []

        errs ->
            [ td [ style [ ( "color", "red" ), ( "border", "none" ) ] ]
                [ text <| String.join "; " errs ]
            ]
