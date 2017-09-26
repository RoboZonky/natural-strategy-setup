module View.Investment exposing (form)

import AllDict
import Bootstrap.Accordion as Accordion
import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Data.Filter.Conditions.Rating exposing (Rating, ratingToString)
import Data.Investment as Investment exposing (InvestmentsPerRating)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onSubmit)
import RangeSlider
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
                , investmentOverridesForm invOverrides
                ]
            ]
        }


defaultInvestmentForm : Investment.Size -> Card.BlockItem Msg
defaultInvestmentForm size =
    Card.custom <|
        Form.formInline [ onSubmit NoOp ]
            [ span [ class "mr-2" ] [ text "Běžná výše investice je" ]
            , Html.map ChangeDefaultInvestment <| RangeSlider.view size
            ]


investmentOverridesForm : InvestmentsPerRating -> Card.BlockItem Msg
investmentOverridesForm investmentsPerRating =
    Card.custom <|
        div []
            [ text "Pokud si přejete, aby se výše investice do půjček jednotlivých ratingů lišily od běžné výše, upravte je pomocí posuvníků"
            , investmentOverridesSliders investmentsPerRating
            ]


investmentOverridesSliders : InvestmentsPerRating -> Html Msg
investmentOverridesSliders investmentsPerRating =
    AllDict.toList investmentsPerRating
        |> List.map (\( rating, sliderState ) -> investmentSlider rating sliderState)
        |> div []


investmentSlider : Rating -> Investment.Size -> Html Msg
investmentSlider rating sliderState =
    Form.formInline [ onSubmit NoOp ]
        [ div [ style [ ( "width", "50px" ) ] ] [ text <| ratingToString rating ]
        , Html.map (ChangeInvestment rating) <| RangeSlider.view sliderState
        ]
