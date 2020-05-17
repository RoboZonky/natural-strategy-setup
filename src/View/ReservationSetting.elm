module View.ReservationSetting exposing (form)

import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Radio as Radio
import Data.ReservationSetting as ReservationSetting exposing (ReservationSetting(..))
import DomId exposing (DomId)
import Html exposing (Html)
import Html.Attributes exposing (class, href, target)


form : (ReservationSetting -> msg) -> ReservationSetting -> CardBlock.Item msg
form onChange reservationSetting =
    Fieldset.config
        |> Fieldset.asGroup
        |> Fieldset.legend [] [ Html.text "Rezervační systém ", helpLink ]
        |> Fieldset.children
            [ reservationRadio onChange "reservationIgnore" Ignore reservationSetting
            , reservationRadio onChange "reservationAcceptMatching" AcceptMatching reservationSetting
            ]
        |> Fieldset.view
        |> CardBlock.custom


helpLink : Html a
helpLink =
    Html.a
        [ href "https://github.com/RoboZonky/natural-strategy-setup/blob/master/docs/ReservationSystem.md"
        , class "fieldset-legend-help"
        , target "_blank"
        ]
        [ Html.text "Nápověda" ]


reservationRadio : (ReservationSetting -> msg) -> DomId -> ReservationSetting -> ReservationSetting -> Html msg
reservationRadio onChange domId thisRadioSetting currentReservationSetting =
    Radio.radio
        [ Radio.id domId
        , Radio.checked (currentReservationSetting == thisRadioSetting)
        , Radio.name "reservationSystem"
        , Radio.onClick (onChange thisRadioSetting)
        ]
        (toRadioLabel thisRadioSetting)


toRadioLabel : ReservationSetting -> String
toRadioLabel =
    String.dropLeft (String.length "Robot má ") << ReservationSetting.render
