module View.PortfolioStructure exposing (defaultPortfolioForm, form)

import AllDict as Dict
import Bootstrap.Accordion as Accordion
import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Table as Table
import Data.Filter.Condition.Rating as Rating
import Data.Portfolio as Portfolio exposing (Portfolio(..))
import Data.PortfolioStructure exposing (PortfolioShare, PortfolioShares, Share)
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class, size, style, value)
import Html.Events exposing (onSubmit)
import Types exposing (..)
import Util


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
    Card.custom <|
        Form.formInline [ onSubmit NoOp ]
            [ text "Robot má udržovat ", defaultPortfolioSelect, text " portfolio." ]


defaultPortfolioSelect : Html Msg
defaultPortfolioSelect =
    let
        optionList =
            List.map
                (\portfolio ->
                    Select.item
                        [ value (toString portfolio) ]
                        [ text (Portfolio.toString portfolio) ]
                )
                [ Conservative, Balanced, Progressive, Empty ]
    in
    Select.select
        [ Select.small
        , Select.onChange (PortfolioChanged << Portfolio.fromString)
        , Select.attrs [ class "mx-1" ]
        ]
        optionList


ratingSharesTable : Portfolio -> PortfolioShares -> Card.BlockItem Msg
ratingSharesTable portfolio shares =
    let
        ( rowRenderingFunction, tableDescription ) =
            case portfolio of
                Empty ->
                    ( portfolioShareEditableRow, "Nastavte" )

                _ ->
                    ( portfolioShareReadOnlyRow, "Následující tabulka ukazuje" )

        thead =
            Table.thead [ Table.defaultHead ]
                [ Table.tr []
                    [ Table.th [] [ text "Rating" ]
                    , Table.th [] [ text "Od (%)" ]
                    , Table.th [] [ text "Do (%)" ]
                    ]
                ]

        tbody =
            Table.tbody [] <| List.map rowRenderingFunction <| Dict.toList shares

        sumOfShareMinimums =
            Dict.foldr (\_ ( min, _ ) acc -> min + acc) 0 shares

        validationErrors =
            if sumOfShareMinimums /= 100 then
                [ div [ style [ ( "color", "red" ) ] ]
                    [ text <| "Součet minim musí být přesně 100% (teď je " ++ toString sumOfShareMinimums ++ "%)" ]
                ]
            else
                [ div [ style [ ( "height", "2em" ) ] ]
                    [{- TODO (Issue #8) this empty div is hack to make the table validation error visible.
                        When user selects "empty" portfolio, the size of table changes,
                        which makes the validation error below it invisible (because the containing
                        accordion's card's height fits the height of its content when it's expanded)
                     -}
                    ]
                ]
    in
    Card.custom <|
        div [] <|
            [ text <| tableDescription ++ " požadovaný procentuální podíl aktuální zůstatkové částky investovaný do půjček v daném ratingu"
            , Table.table
                { options = [ Table.bordered, Table.small, Table.attr (style [ ( "border", "none" ) ]) ]
                , thead = thead
                , tbody = tbody
                }
            ]
                ++ validationErrors


portfolioShareReadOnlyRow : PortfolioShare -> Table.Row Msg
portfolioShareReadOnlyRow ( rtg, ( mi, mx ) ) =
    Table.tr []
        [ Table.td [] [ text <| Rating.ratingToString rtg ]
        , Table.td [] [ text <| toString mi ]
        , Table.td [] [ text <| toString mx ]
        , validationErrors ( mi, mx )
        ]


portfolioShareEditableRow : PortfolioShare -> Table.Row Msg
portfolioShareEditableRow ( rtg, ( mi, mx ) ) =
    let
        inputCell val msg =
            Input.number
                [ Input.small
                , Input.value (Util.zeroToEmpty val)
                , Input.onInput msg
                , Input.attrs <| [ size 2, Attr.min "0", Attr.max "100" ]
                ]
    in
    Table.tr []
        [ Table.td [] [ text <| Rating.ratingToString rtg ]
        , Table.td [] [ inputCell mi (ChangePortfolioShareMin rtg) ]
        , Table.td [] [ inputCell mx (ChangePortfolioShareMax rtg) ]
        , validationErrors ( mi, mx )
        ]


validationErrors : Share -> Table.Cell Msg
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
    validationCell <| String.join "; " <| minGtMax ++ maxOver100


validationCell : String -> Table.Cell Msg
validationCell errorText =
    Table.td
        [ Table.cellAttr
            (style
                {- TODO (Issue #9) hack to make the first three columns NOT take up whole width of accordion card we ALWAYS
                   (even when there's not validation error) display extra column with validation error occupying 70% of width.
                -}
                [ ( "width", "70%" )
                , ( "border", "none" )
                , ( "color", "red" )
                ]
            )
        ]
        [ text errorText ]
