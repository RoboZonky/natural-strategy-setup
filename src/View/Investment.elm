module View.Investment exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Alert as Alert
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form as Form
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Utilities.Spacing as Spacing
import Data.Filter.Conditions.Rating as Rating exposing (Rating)
import Data.Investment as Investment exposing (InvestmentsPerRating)
import Dict.Any
import Html exposing (Html, a, b, div, span, strong, text)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onSubmit)
import RangeSlider
import Types exposing (Msg(..))


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


defaultInvestmentForm : Investment.Size -> CardBlock.Item Msg
defaultInvestmentForm invDefault =
    CardBlock.custom <|
        Form.formInline [ onSubmit NoOp ]
            [ span [ Spacing.mr2 ] [ text "Běžná výše investice je" ]
            , Html.map ChangeDefaultInvestment <| RangeSlider.view invDefault
            ]


investmentOverridesForm : Investment.Size -> InvestmentsPerRating -> CardBlock.Item Msg
investmentOverridesForm invDefault invOverrides =
    CardBlock.custom <|
        div []
            [ text "Pokud si přejete, aby se výše investice do půjček lišily od běžné výše na základě rizikových kategorií, upravte je pomocí posuvníků"
            , Grid.row []
                [ Grid.col [ Col.xs6 ] [ investmentOverridesSliders invOverrides ]
                , Grid.col [ Col.xs6 ] [ warningWhenSizeExceeds5K invDefault invOverrides ]
                ]
            ]


warningWhenSizeExceeds5K : Investment.Size -> InvestmentsPerRating -> Html a
warningWhenSizeExceeds5K invDefault invOverrides =
    if Investment.anyInvestmentExceeds5k invDefault invOverrides then
        Alert.simpleDanger []
            [ strong [] [ text "Upozornění. " ]
            , text "Maximální výše investice na Zonky se řídí"
            , a [ href "https://zonky.cz/downloads/Zonky_Parametry_castek_pro_investovani.pdf" ] [ text " následujícími pravidly" ]
            , text ". Nastavíte-li částku vyšší, robot bude investovat pouze maximální povolenou částku."
            ]

    else
        text ""


investmentOverridesSliders : InvestmentsPerRating -> Html Msg
investmentOverridesSliders invOverrides =
    Dict.Any.toList invOverrides
        |> List.map (\( rating, sliderState ) -> investmentSlider rating sliderState)
        |> div []


investmentSlider : Rating -> Investment.Size -> Html Msg
investmentSlider rating sliderState =
    Form.formInline [ onSubmit NoOp ]
        [ b [ style "width" "100px" ] [ text <| Rating.showInterestPercent rating ]
        , Html.map (ChangeInvestment rating) <| RangeSlider.view sliderState
        ]
