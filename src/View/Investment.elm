module View.Investment exposing (investmentForm)

import AllDict exposing (AllDict)
import Data.Investment as Investment exposing (InvestmentsPerRating)
import Data.Rating as Rating exposing (Rating)
import Html exposing (Html, button, div, h2, input, p, table, td, text, th, tr)
import Html.Attributes as Attr exposing (size, style, type_, value)
import Html.Events exposing (onInput)
import Types exposing (..)


defaultInvestmentForm : Investment.Size -> Html Msg
defaultInvestmentForm ( defaultMin, defaultMax ) =
    div []
        [ text "Běžná výše investice je "
        , inputCell defaultMin ChangeDefaultInvestmentMin
        , text " až "
        , inputCell defaultMax ChangeDefaultInvestmentMax
        ]


investmentForm : Investment.Size -> InvestmentsPerRating -> Html Msg
investmentForm invDefault invOverrides =
    div []
        [ h2 [] [ text "Výše investice" ]
        , defaultInvestmentForm invDefault
        , investmentOverridesForm invDefault invOverrides
        ]


investmentOverridesForm : Investment.Size -> InvestmentsPerRating -> Html Msg
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
    p [] <|
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
