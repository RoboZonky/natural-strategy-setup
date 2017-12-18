module View.Investment exposing (form)

import AllDict
import Bootstrap.Accordion as Accordion
import Bootstrap.Alert as Alert
import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Data.Filter.Conditions.Rating exposing (Rating, ratingToString)
import Data.Investment as Investment exposing (InvestmentsPerRating)
import Html exposing (Html, a, div, span, strong, text)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onSubmit)
import RangeSlider
import Types exposing (Msg(ChangeDefaultInvestment, ChangeInvestment, NoOp))


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
defaultInvestmentForm invDefault =
    Card.custom <|
        Form.formInline [ onSubmit NoOp ]
            [ span [ class "mr-2" ] [ text "Běžná výše investice je" ]
            , Html.map ChangeDefaultInvestment <| RangeSlider.view invDefault
            ]


investmentOverridesForm : Investment.Size -> InvestmentsPerRating -> Card.BlockItem Msg
investmentOverridesForm invDefault invOverrides =
    Card.custom <|
        div []
            [ text "Pokud si přejete, aby se výše investice do půjček jednotlivých ratingů lišily od běžné výše, upravte je pomocí posuvníků"
            , Grid.row []
                [ Grid.col [ Col.xs6 ] [ investmentOverridesSliders invOverrides ]
                , Grid.col [ Col.xs6 ] [ warningWhenSizeExceeds5K invDefault invOverrides ]
                ]
            ]


warningWhenSizeExceeds5K : Investment.Size -> InvestmentsPerRating -> Html a
warningWhenSizeExceeds5K invDefault invOverrides =
    if Investment.anyInvestmentExceeds5k invDefault invOverrides then
        Alert.danger
            [ strong [] [ text "Upozornění. " ]
            , text "Maximální výše investice na Zonky se řídí"
            , a [ href "https://zonky.cz/downloads/Zonky_Parametry_castek_pro_investovani.pdf" ] [ text " následujícími pravidly" ]
            , text ". Nastavíte-li částku vyšší, robot bude investovat pouze maximální povolenou částku."
            ]
    else
        text ""


investmentOverridesSliders : InvestmentsPerRating -> Html Msg
investmentOverridesSliders invOverrides =
    AllDict.toList invOverrides
        |> List.map (\( rating, sliderState ) -> investmentSlider rating sliderState)
        |> div []


investmentSlider : Rating -> Investment.Size -> Html Msg
investmentSlider rating sliderState =
    Form.formInline [ onSubmit NoOp ]
        [ div [ style [ ( "width", "50px" ) ] ] [ text <| ratingToString rating ]
        , Html.map (ChangeInvestment rating) <| RangeSlider.view sliderState
        ]
