module View.Investment exposing (form)

import AllDict exposing (AllDict)
import Bootstrap.Accordion as Accordion
import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Table as Table
import Data.Filter.Conditions.Rating as Rating exposing (Rating)
import Data.Investment as Investment exposing (InvestmentsPerRating)
import Html exposing (Attribute, Html, div, text)
import Html.Attributes as Attr exposing (class, size, style)
import Html.Events exposing (onSubmit)
import Types exposing (..)
import Util


form : Investment.Size -> InvestmentsPerRating -> Accordion.Card Msg
form invDefault invOverrides =
    Accordion.card
        { id = "investmentSizeCard"
        , options = []
        , header = Accordion.headerH4 [] <| Accordion.toggle [] [ text "Výše investice" ]
        , blocks =
            [ Accordion.block []
                [ defaultInvestmentForm invDefault
                , investmentOverridesForm invDefault invOverrides
                ]
            ]
        }


defaultInvestmentForm : Investment.Size -> Card.BlockItem Msg
defaultInvestmentForm ( defaultMin, defaultMax ) =
    Card.custom <|
        Form.formInline [ onSubmit NoOp ]
            [ text "Běžná výše investice je"
            , inputCell [ class "mx-1" ] defaultMin ChangeDefaultInvestmentMin
            , text "až"
            , inputCell [ class "mx-1" ] defaultMax ChangeDefaultInvestmentMax
            , text "Kč."
            ]


investmentOverridesForm : Investment.Size -> InvestmentsPerRating -> Card.BlockItem Msg
investmentOverridesForm default overrides =
    Card.custom <|
        div []
            [ text "Pokud si přejete, aby se výše investice lišily na základě ratingu půjčky, upravte je v následující tabulce"
            , investmentOverridesTable default overrides
            ]


investmentOverridesTable : Investment.Size -> InvestmentsPerRating -> Html Msg
investmentOverridesTable default overrides =
    let
        thead =
            Table.thead [ Table.defaultHead ]
                [ Table.tr []
                    [ Table.th [] [ text "Rating" ]
                    , Table.th [] [ text "Od (Kč)" ]
                    , Table.th [] [ text "Do (Kč)" ]
                    ]
                ]

        tbody =
            Table.tbody [] <| List.map (investmentRow default overrides) Rating.allRatings
    in
    Table.table
        { options = [ Table.bordered, Table.small, Table.attr (style [ ( "border", "none" ) ]) ]
        , thead = thead
        , tbody = tbody
        }


investmentRow : Investment.Size -> InvestmentsPerRating -> Rating -> Table.Row Msg
investmentRow invDefault invOverrides rating =
    let
        ( mi, mx ) =
            Maybe.withDefault invDefault <| AllDict.get rating invOverrides

        validationError =
            if mi > mx then
                "Minimum nesmí být větší než maximum"
            else
                ""
    in
    Table.tr []
        [ Table.td [] [ text <| Rating.ratingToString rating ]
        , Table.td [] [ inputCell [] mi (ChangeInvestmentMin rating) ]
        , Table.td [] [ inputCell [] mx (ChangeInvestmentMax rating) ]
        , validationCell validationError
        ]


validationCell : String -> Table.Cell Msg
validationCell errorText =
    Table.td
        [ Table.cellAttr
            (style
                {- TODO (Issue #9) hack to make the first three columns NOT take up whole width of accordion card we ALWAYS
                   (even when there's not validation error) display extra column with validation error occupying 65% of width.
                -}
                [ ( "width", "65%" )
                , ( "border", "none" )
                , ( "color", "red" )
                ]
            )
        ]
        [ text errorText ]


inputCell : List (Attribute Msg) -> Int -> (String -> Msg) -> Html Msg
inputCell extraAttrs val msg =
    Input.number
        [ Input.small
        , Input.value <| Util.zeroToEmpty val
        , Input.onInput msg
        , Input.attrs <| extraAttrs ++ [ size 4, Attr.min "0", Attr.max "1000000" ]
        ]
