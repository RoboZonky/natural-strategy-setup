module View.InvestmentForm exposing (investmentForm)

import Data.Investment exposing (InvestmentPerRating)
import Html exposing (Html, button, div, h2, input, table, td, text, tr)
import Html.Attributes exposing (type_)
import Types exposing (..)


defaultInvestmentForm : Html Msg
defaultInvestmentForm =
    div []
        [ text "Běžná výše investice je "
        , input [ type_ "number" ] []
        , text " až "
        , input [ type_ "number" ] []
        ]


investmentForm : List InvestmentPerRating -> Html Msg
investmentForm iprs =
    div []
        [ h2 [] [ text "Výše investice" ]
        , defaultInvestmentForm
        , investmentOverridesForm
        ]


investmentOverridesForm : Html Msg
investmentOverridesForm =
    table []
        [ tr []
            [ td [] [ text "a" ]
            ]
        , tr
            []
            [ td [] [ text "b" ] ]
        ]
