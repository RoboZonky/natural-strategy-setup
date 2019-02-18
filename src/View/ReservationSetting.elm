module View.ReservationSetting exposing (form)

import Bootstrap.Card.Block as CardBlock
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Data.ReservationSetting as ReservationSetting exposing (ReservationSetting(..))
import Html exposing (a, text)
import Html.Attributes exposing (href)
import Types exposing (Msg(..))
import View.EnumSelect as EnumSelect


form : ReservationSetting -> CardBlock.Item Msg
form reservationSetting =
    Fieldset.config
        |> Fieldset.asGroup
        |> Fieldset.legend [] [ text "Rezervační systém" ]
        |> Fieldset.children
            [ Grid.row []
                [ Grid.col [ Col.xs10 ]
                    [ EnumSelect.from
                        { enumValues = ReservationSetting.allSettings
                        , valuePickedMessage = SetReservationSetting
                        , showVisibleLabel = String.dropLeft (String.length "Robot má ") << ReservationSetting.render
                        , defaultOption = EnumSelect.DefaultOption reservationSetting
                        , enabled = True
                        }
                    ]
                , Grid.col [ Col.xs2 ]
                    [ a [ href "https://github.com/RoboZonky/robozonky/wiki/Rezervační-systém-v-RoboZonky" ] [ text "Nápověda" ] ]
                ]
            ]
        |> Fieldset.view
        |> CardBlock.custom
