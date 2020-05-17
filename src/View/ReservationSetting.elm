module View.ReservationSetting exposing (form)

import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Radio as Radio
import Data.ReservationSetting as ReservationSetting exposing (ReservationSetting(..))
import DomId exposing (DomId)
import Html exposing (Html)
import Html.Attributes exposing (class, href, target)
import Types exposing (Msg(..))


form : ReservationSetting -> CardBlock.Item Msg
form reservationSetting =
    Fieldset.config
        |> Fieldset.asGroup
        |> Fieldset.legend [] [ Html.text "Rezervační systém ", helpLink ]
        |> Fieldset.children
            [ reservationRadio "reservationIgnore" Ignore reservationSetting
            , reservationRadio "reservationAcceptMatching" AcceptMatching reservationSetting
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


reservationRadio : DomId -> ReservationSetting -> ReservationSetting -> Html Msg
reservationRadio domId thisRadioSetting currentReservationSetting =
    Radio.radio
        [ Radio.id domId
        , Radio.checked (currentReservationSetting == thisRadioSetting)
        , Radio.name "reservationSystem"
        , Radio.onClick (ReservationSettingChanged thisRadioSetting)
        ]
        (toRadioLabel thisRadioSetting)


toRadioLabel : ReservationSetting -> String
toRadioLabel =
    String.dropLeft (String.length "Robot má ") << ReservationSetting.render
