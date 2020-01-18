module View.Investment exposing (form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Alert as Alert
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form as Form
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Utilities.Spacing as Spacing
import Data.Filter.Conditions.Rating as Rating exposing (Rating, ratingDictToList)
import Data.Investment as Investment exposing (InvestmentsPerRating, PrimaryInvestmentSize)
import Html exposing (Html, a, b, div, span, strong, text)
import Html.Attributes as Attr exposing (class, href, style)
import Html.Events as Events exposing (onSubmit)
import Json.Decode as Decode
import Types exposing (Msg(..))


form : PrimaryInvestmentSize -> InvestmentsPerRating -> Accordion.Card Msg
form invDefault invOverrides =
    Accordion.card
        { id = "investmentSizeCard"
        , options = []
        , header = Accordion.headerH4 [] <| Accordion.toggle [] [ text "Výše investice" ]
        , blocks =
            [ Accordion.block [ CardBlock.attrs [ class "tab-with-sliders" ] ]
                [ defaultInvestmentForm invDefault
                , investmentOverridesForm invDefault invOverrides
                ]
            ]
        }


defaultInvestmentForm : PrimaryInvestmentSize -> CardBlock.Item Msg
defaultInvestmentForm invDefault =
    CardBlock.custom <|
        Form.formInline [ onSubmit NoOp ]
            [ span [ Spacing.mr2 ] [ text "Běžná výše investice je" ]
            , Html.map ChangeDefaultInvestment <| sliderView invDefault

            -- TODO show slider value
            -- TODO style the sliders
            ]


investmentOverridesForm : PrimaryInvestmentSize -> InvestmentsPerRating -> CardBlock.Item Msg
investmentOverridesForm invDefault invOverrides =
    CardBlock.custom <|
        div []
            [ text "Pokud si přejete, aby se výše investice do půjček lišily od běžné výše na základě rizikových kategorií, upravte je pomocí posuvníků"
            , Grid.row []
                [ Grid.col [ Col.xs6 ] [ investmentOverridesSliders invOverrides ]
                , Grid.col [ Col.xs6 ] [ warningWhenSizeExceeds5K invDefault invOverrides ]
                ]
            ]


warningWhenSizeExceeds5K : PrimaryInvestmentSize -> InvestmentsPerRating -> Html a
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
    ratingDictToList invOverrides
        |> List.map investmentSlider
        |> div []


investmentSlider : ( Rating, PrimaryInvestmentSize ) -> Html Msg
investmentSlider ( rating, sliderState ) =
    Form.formInline [ onSubmit NoOp ]
        [ b [ style "width" "105px" ] [ text <| Rating.showInterestPercent rating ]
        , Html.map (ChangeInvestment rating) <| sliderView sliderState

        -- TODO show slider value
        ]


sliderView : PrimaryInvestmentSize -> Html Investment.Msg
sliderView pis =
    Html.input
        [ Attr.type_ "range"
        , Attr.class "primary-investment-slider"
        , Attr.min "0"
        , Attr.max "20000"
        , Attr.step "200"
        , Attr.value <| String.fromInt <| Investment.toInt pis
        , Events.on "input"
            (Events.targetValue
                |> Decode.andThen valueDecoder
            )
        ]
        []


valueDecoder : String -> Decode.Decoder Investment.Msg
valueDecoder str =
    case String.toInt str of
        Nothing ->
            Decode.fail "ignore"

        Just i ->
            Decode.succeed (Investment.SetValue i)
