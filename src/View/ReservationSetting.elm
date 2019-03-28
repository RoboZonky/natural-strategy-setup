module View.ReservationSetting exposing (form)

import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Radio as Radio
import Data.ReservationSetting as ReservationSetting exposing (ReservationSetting(..))
import Html exposing (Html, a, text)
import Html.Attributes exposing (class, href)
import Types exposing (Msg(..))


form : ReservationSetting -> CardBlock.Item Msg
form reservationSetting =
    Fieldset.config
        |> Fieldset.asGroup
        |> Fieldset.legend [] [ text "Rezervační systém ", helpLink ]
        |> Fieldset.children
            [ reservationRadio "reservationIgnore" Ignore reservationSetting
            , reservationRadio "reservationAcceptMatching" AcceptMatching reservationSetting
            ]
        |> Fieldset.view
        |> CardBlock.custom


helpLink : Html a
helpLink =
    a
        [ href "https://github.com/RoboZonky/natural-strategy-setup/blob/master/docs/ReservationSystem.md"
        , class "fieldset-legend-help"
        ]
        [ text "Nápověda" ]


reservationRadio : String -> ReservationSetting -> ReservationSetting -> Html Msg
reservationRadio domId thisRadioSetting currentReservationSetting =
    Radio.radio
        [ Radio.id domId
        , Radio.checked (currentReservationSetting == thisRadioSetting)
        , Radio.name "reservationSystem"
        , Radio.onClick (SetReservationSetting thisRadioSetting)
        ]
        (toRadioLabel thisRadioSetting)


toRadioLabel : ReservationSetting -> String
toRadioLabel =
    String.dropLeft (String.length "Robot má ") << ReservationSetting.render