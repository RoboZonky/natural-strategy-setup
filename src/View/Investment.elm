module View.Investment exposing (form)

import AllDict exposing (AllDict)
import Bootstrap.Accordion as Accordion
import Bootstrap.Card as Card
import Data.Investment as Investment exposing (InvestmentsPerRating)
import Data.Rating as Rating exposing (Rating)
import Html exposing (Html, button, div, h2, input, p, table, td, text, th, tr)
import Html.Attributes as Attr exposing (size, style, type_, value)
import Html.Events exposing (onInput)
import Types exposing (..)


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
        div []
            [ text "Běžná výše investice je "
            , inputCell defaultMin ChangeDefaultInvestmentMin
            , text " až "
            , inputCell defaultMax ChangeDefaultInvestmentMax
            , text " Kč."
            ]


investmentOverridesForm : Investment.Size -> InvestmentsPerRating -> Card.BlockItem Msg
investmentOverridesForm default overrides =
    let
        headerRow =
            tr []
                [ th [] [ text "Rating" ]
                , th [] [ text "od (Kč)" ]
                , th [] [ text "do (Kč)" ]
                ]

        dataRows =
            List.map (investmentRow default overrides) Rating.allRatings
    in
    Card.custom <|
        div []
            [ text "Pokud si přejete, aby se výše investice lišily na základě ratingu půjčky, upravte je v následující tabulce"
            , table [] (headerRow :: dataRows)
            ]


investmentRow : Investment.Size -> InvestmentsPerRating -> Rating -> Html Msg
investmentRow invDefault invOverrides rating =
    let
        ( mi, mx ) =
            Maybe.withDefault invDefault <| AllDict.get rating invOverrides

        validationError =
            if mi > mx then
                [ td [ style [ ( "color", "red" ), ( "border", "none" ) ] ]
                    [ text "Minimum musí být menší nebo rovno než maximum" ]
                ]
            else
                []
    in
    tr [] <|
        [ td [] [ text <| Rating.ratingToString rating ]
        , td [] [ inputCell mi (ChangeInvestmentMin rating) ]
        , td [] [ inputCell mx (ChangeInvestmentMax rating) ]
        ]
            ++ validationError


inputCell : Int -> (String -> Msg) -> Html Msg
inputCell val msg =
    input [ size 4, type_ "number", Attr.min "0", Attr.max "1000000", value (toString val), onInput msg ] []
