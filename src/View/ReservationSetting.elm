module View.ReservationSetting exposing (form)

import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form.Fieldset as Fieldset
import Data.ReservationSetting as ReservationSetting exposing (ReservationSetting(..))
import Html exposing (text)
import Types exposing (Msg(..))
import View.EnumSelect as EnumSelect


form : ReservationSetting -> CardBlock.Item Msg
form reservationSetting =
    Fieldset.config
        |> Fieldset.asGroup
        |> Fieldset.legend [] [ text "Rezervační systém" ]
        |> Fieldset.children
            [ EnumSelect.from
                { enumValues = ReservationSetting.allSettings
                , valuePickedMessage = SetReservationSetting
                , showVisibleLabel = String.dropLeft (String.length "Robot má ") << ReservationSetting.render
                , defaultOption = EnumSelect.DefaultOption reservationSetting
                , enabled = True
                }
            ]
        |> Fieldset.view
        |> CardBlock.custom
