module View.Filter exposing (filterListView)

import Bootstrap.Badge as Badge
import Bootstrap.Card as Card
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Utilities.Spacing as Spacing
import Data.Filter as Filter exposing (FilteredItem(..), MarketplaceFilter)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


filterListView : (Int -> msg) -> List MarketplaceFilter -> Html msg
filterListView removeFilterByIndexMsg filters =
    case filters of
        [] ->
            Html.text ""

        nonEmptyList ->
            List.indexedMap (viewFilter removeFilterByIndexMsg) nonEmptyList
                |> List.intersperse orConnective
                |> Html.div [ Spacing.p2 ]


viewFilter : (Int -> msg) -> Int -> MarketplaceFilter -> Html msg
viewFilter removeFilterByIndexMsg index mf =
    let
        removeButton =
            Html.span [ onClick (removeFilterByIndexMsg index), class "float-right" ]
                [ Html.text "✖" ]

        filterText =
            Filter.filterTextView mf
    in
    Card.config []
        |> Card.block [ CardBlock.attrs [ Spacing.p2 ] ]
            [ CardBlock.custom <|
                Grid.row []
                    [ Grid.col [ Col.xs11 ] [ filterText ]
                    , Grid.col [ Col.xs1 ] [ removeButton ]
                    ]
            ]
        |> Card.view


orConnective : Html a
orConnective =
    Html.div [ class "or-connective" ]
        [ Badge.pillSecondary [] [ Html.text "a nebo" ]
        ]
