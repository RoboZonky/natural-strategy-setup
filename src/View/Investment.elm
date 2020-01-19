module View.Investment exposing (Config, form)

import Bootstrap.Accordion as Accordion
import Bootstrap.Alert as Alert
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form as Form
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Utilities.Spacing as Spacing
import Data.Filter.Conditions.Rating as Rating exposing (Rating, ratingDictToList)
import Data.Investment as Investment exposing (InvestmentsPerRating, Size)
import Html exposing (Html, a, b, div, span, strong, text)
import Html.Attributes as Attr exposing (class, href, style)
import Html.Events as Events exposing (onSubmit)
import Json.Decode as Decode
import Types exposing (Msg(..))


type alias Config msg =
    { onDefaultInvestmentChange : Investment.Msg -> msg
    , onInvestmentChange : Rating -> Investment.Msg -> msg
    , noOp : msg
    }


form : Config msg -> Size -> InvestmentsPerRating -> Accordion.Card msg
form config invDefault invOverrides =
    Accordion.card
        { id = "investmentSizeCard"
        , options = []
        , header = Accordion.headerH4 [] <| Accordion.toggle [] [ text "Výše investice" ]
        , blocks =
            [ Accordion.block [ CardBlock.attrs [ class "tab-with-sliders" ] ]
                [ defaultInvestmentForm config invDefault
                , investmentOverridesForm config invDefault invOverrides
                ]
            ]
        }


defaultInvestmentForm : Config msg -> Size -> CardBlock.Item msg
defaultInvestmentForm config invDefault =
    CardBlock.custom <|
        Form.formInline [ onSubmit config.noOp ]
            [ span [ Spacing.mr2 ] [ text "Běžná výše investice je" ]
            , Html.map config.onDefaultInvestmentChange <| sliderView invDefault
            , span [ Spacing.ml2 ] [ Html.text <| Investment.toCzkString invDefault ]

            -- TODO style the sliders
            ]


investmentOverridesForm : Config msg -> Size -> InvestmentsPerRating -> CardBlock.Item msg
investmentOverridesForm config invDefault invOverrides =
    CardBlock.custom <|
        div []
            [ text "Pokud si přejete, aby se výše investice do půjček lišily od běžné výše na základě rizikových kategorií, upravte je pomocí posuvníků"
            , Grid.row []
                [ Grid.col [ Col.xs6 ] [ investmentOverridesSliders config invOverrides ]
                , Grid.col [ Col.xs6 ] [ warningWhenSizeExceeds5K invDefault invOverrides ]
                ]
            ]


warningWhenSizeExceeds5K : Size -> InvestmentsPerRating -> Html a
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


investmentOverridesSliders : Config msg -> InvestmentsPerRating -> Html msg
investmentOverridesSliders config invOverrides =
    ratingDictToList invOverrides
        |> List.map (investmentSlider config)
        |> div []


investmentSlider : Config msg -> ( Rating, Size ) -> Html msg
investmentSlider config ( rating, invSize ) =
    Form.formInline [ onSubmit config.noOp ]
        [ b [ style "width" "105px" ] [ text <| Rating.showInterestPercent rating ]
        , Html.map (config.onInvestmentChange rating) <| sliderView invSize
        , span [ Spacing.ml2 ] [ Html.text <| Investment.toCzkString invSize ]

        -- TODO show slider value
        ]


sliderView : Size -> Html Investment.Msg
sliderView pis =
    Html.input
        [ Attr.type_ "range"
        , Attr.class "primary-investment-slider"
        , Attr.min "200"
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
