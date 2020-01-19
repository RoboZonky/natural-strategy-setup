module Data.Investment exposing
    ( Config
    , InvestmentsPerRating
    , Msg(..)
    , Size
    , decoder
    , defaultInvestmentsPerRating
    , defaultSize
    , encode
    , encodeSize
    , form
    , fromInt
    , investmentSizeEqual
    , investmentsPerRatingEqual
    , renderInvestmentsPrimary
    , renderInvestmentsSecondary
    , renderSizePrimary
    , renderSizeSecondary
    , sizeDecoder
    , update
    )

import Bootstrap.Accordion as Accordion
import Bootstrap.Alert as Alert
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form as Form
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Utilities.Spacing as Spacing
import Data.Filter.Conditions.Rating as Rating exposing (Rating, ratingDictToList)
import Dict.Any exposing (AnyDict)
import Html exposing (Html, a, b, div, span, strong, text)
import Html.Attributes as Attr exposing (class, href, style)
import Html.Events as Events exposing (onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util


type alias InvestmentsPerRating =
    AnyDict Int Rating Size


defaultInvestmentsPerRating : Size -> InvestmentsPerRating
defaultInvestmentsPerRating initialSize =
    Rating.initRatingDict <| List.map (\r -> ( r, initialSize )) Rating.allRatings


type Size
    = Size Int


defaultSize : Size
defaultSize =
    Size 200


fromInt : Int -> Size
fromInt =
    Size


toInt : Size -> Int
toInt (Size sz) =
    sz


toCzkString : Size -> String
toCzkString (Size sz) =
    String.fromInt sz ++ " Kč"


encodeSize : Size -> Value
encodeSize (Size sz) =
    Encode.int sz


sizeDecoder : Decoder Size
sizeDecoder =
    Decode.map Size Decode.int


type Msg
    = SetValue Int


update : Msg -> Size -> Size
update msg (Size _) =
    case msg of
        SetValue newValue ->
            Size newValue


renderSizePrimary : Size -> String
renderSizePrimary size =
    "Robot má investovat do úvěrů po " ++ toCzkString size ++ "."


renderSizeSecondary : Size -> String
renderSizeSecondary size =
    "Robot má nakupovat participace nejvýše za " ++ toCzkString size ++ "."


renderSizePerRatingPrimary : ( Rating, Size ) -> String
renderSizePerRatingPrimary ( rating, size ) =
    "Do úvěrů s úročením " ++ Rating.showInterestPercent rating ++ " investovat po " ++ toCzkString size ++ "."


renderSizePerRatingSecondary : ( Rating, Size ) -> String
renderSizePerRatingSecondary ( rating, size ) =
    "Participace s úročením " ++ Rating.showInterestPercent rating ++ " nakupovat nejvýše za " ++ toCzkString size ++ "."


renderInvestmentsPrimary : Size -> InvestmentsPerRating -> String
renderInvestmentsPrimary defaultSize_ investments =
    if Dict.Any.isEmpty investments then
        ""

    else
        Rating.ratingDictToList investments
            --filter our sizes equal to default size
            |> List.filter (\( _, invSize ) -> invSize /= defaultSize_)
            |> List.map renderSizePerRatingPrimary
            |> Util.renderNonemptySection "\n- Výše investice"


renderInvestmentsSecondary : Size -> InvestmentsPerRating -> String
renderInvestmentsSecondary defaultSize_ investments =
    if Dict.Any.isEmpty investments then
        ""

    else
        Rating.ratingDictToList investments
            --filter our sizes equal to default size
            |> List.filter (\( _, invSize ) -> invSize /= defaultSize_)
            |> List.map renderSizePerRatingSecondary
            |> Util.renderNonemptySection "\n- Výše nákupu"


anyInvestmentExceeds5k : Size -> InvestmentsPerRating -> Bool
anyInvestmentExceeds5k default overrides =
    default
        :: Dict.Any.values overrides
        |> List.map toInt
        |> List.filter (\x -> x > 5000)
        |> (not << List.isEmpty)


investmentSizeEqual : Size -> Size -> Bool
investmentSizeEqual =
    (==)


investmentsPerRatingEqual : InvestmentsPerRating -> InvestmentsPerRating -> Bool
investmentsPerRatingEqual ipr1 ipr2 =
    Dict.Any.values ipr1 == Dict.Any.values ipr2



-- JSON


encode : InvestmentsPerRating -> Value
encode =
    Rating.ratingDictToList
        >> Encode.list
            (\( _ {- assuming that rating is always sorted in order of rating's toInterestPercent, so just encoding slider states -}, size ) ->
                encodeSize size
            )


decoder : Decoder InvestmentsPerRating
decoder =
    Decode.list sizeDecoder
        |> Decode.andThen
            (\sizes ->
                case sizes of
                    [ _, _, _, _, _, _, _, _, _, _, _ {- 11 values since RZ 5.1.1 -} ] ->
                        List.map2 Tuple.pair Rating.allRatings sizes
                            |> Rating.initRatingDict
                            |> Decode.succeed

                    _ ->
                        "Unexpected number of values when decoding InvestmentPerRating: "
                            ++ String.fromInt (List.length sizes)
                            |> Decode.fail
            )



-- VIEW


type alias Config msg =
    { accordionId : String
    , accordionHeader : String
    , marketplaceNotEnabled : Html msg
    , defaultLabel : String
    , overrideLabel : String
    , onDefaultInvestmentChange : Msg -> msg
    , onInvestmentChange : Rating -> Msg -> msg
    , noOp : msg
    }


form : Config msg -> Bool -> Size -> InvestmentsPerRating -> Accordion.Card msg
form config buyingEnabled invDefault invOverrides =
    Accordion.card
        { id = config.accordionId
        , options = []
        , header = Accordion.headerH4 [] <| Accordion.toggle [] [ text config.accordionHeader ]
        , blocks =
            if buyingEnabled then
                [ Accordion.block [ CardBlock.attrs [ class "tab-with-sliders" ] ]
                    [ defaultInvestmentForm config invDefault
                    , investmentOverridesForm config invDefault invOverrides
                    ]
                ]

            else
                [ Accordion.block [] [ CardBlock.custom config.marketplaceNotEnabled ] ]
        }


defaultInvestmentForm : Config msg -> Size -> CardBlock.Item msg
defaultInvestmentForm config invDefault =
    CardBlock.custom <|
        Form.formInline [ onSubmit config.noOp ]
            [ span [ Spacing.mr2 ] [ text config.defaultLabel ]
            , Html.map config.onDefaultInvestmentChange <| sliderView invDefault
            , span [ Spacing.ml2 ] [ Html.text <| toCzkString invDefault ]
            ]


investmentOverridesForm : Config msg -> Size -> InvestmentsPerRating -> CardBlock.Item msg
investmentOverridesForm config invDefault invOverrides =
    CardBlock.custom <|
        div []
            [ text config.overrideLabel
            , Grid.row []
                [ Grid.col [ Col.xs6 ] [ investmentOverridesSliders config invOverrides ]
                , Grid.col [ Col.xs6 ] [ warningWhenSizeExceeds5K invDefault invOverrides ]
                ]
            ]


warningWhenSizeExceeds5K : Size -> InvestmentsPerRating -> Html a
warningWhenSizeExceeds5K invDefault invOverrides =
    if anyInvestmentExceeds5k invDefault invOverrides then
        Alert.simpleDanger []
            [ strong [] [ text "Upozornění. " ]

            -- TODO parametrize this text
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
        , span [ Spacing.ml2 ] [ Html.text <| toCzkString invSize ]
        ]


sliderView : Size -> Html Msg
sliderView size =
    Html.input
        [ Attr.type_ "range"
        , Attr.class "investment-slider"

        -- I don't know why, but it doesn't work when included in JUST css for ".investment-slider" class
        , Attr.style "-webkit-appearance" "none"
        , Attr.min "200"
        , Attr.max "20000"
        , Attr.step "200"
        , Attr.style "width" "200px"
        , Attr.value <| String.fromInt <| toInt size
        , Events.on "input"
            (Events.targetValue
                |> Decode.andThen valueDecoder
            )
        ]
        []


valueDecoder : String -> Decode.Decoder Msg
valueDecoder str =
    case String.toInt str of
        Nothing ->
            Decode.fail "ignore"

        Just i ->
            Decode.succeed (SetValue i)
