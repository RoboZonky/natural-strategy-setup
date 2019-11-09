module View.Alert exposing (AlertData(..), view)

import Bootstrap.Alert
import Html exposing (Html, a, button, div, li, span, text, ul)
import Html.Attributes exposing (class, href, target, type_)
import Html.Events exposing (onClick)
import Types exposing (Msg(..))
import Url.Builder exposing (crossOrigin, string)


type AlertData
    = SuccessAlert String
    | WarningAlert (List String)
    | ErrorAlert String
    | NoAlert


view : AlertData -> Html Msg
view maybeAlert =
    case maybeAlert of
        SuccessAlert successText ->
            Bootstrap.Alert.simpleSuccess []
                [ text successText
                , closeAlertButton
                ]

        ErrorAlert error ->
            Bootstrap.Alert.simpleDanger []
                [ text "Pokus o načtení strategie z URL se nezdařil. Prosím nahlašte chybu na stránce projektu kliknutím na tento "
                , a [ href (openIssueUrl error) ] [ text "odkaz" ]
                , closeAlertButton
                ]

        WarningAlert warnings ->
            Bootstrap.Alert.simpleWarning []
                [ closeAlertButton
                , div [] [ text <| "Upozornění: Strategii se podařilo obnovit z URL jen částečně." ]
                , div []
                    [ text "Došlo k několika zpětně nekompatibilním změnám ve formátu strategie (viz "
                    , a
                        [ href "https://github.com/RoboZonky/natural-strategy-setup/blob/master/docs/StrategyFormatChangelog.md"
                        , target "_blank"
                        ]
                        [ text "dokumentace" ]
                    , text ")"
                    ]
                , div [] [ text "V důsledku toho musely být některé části vaší konfigurace odstraněny." ]
                , ul [] <| List.map (\warning -> li [] [ text warning ]) warnings
                ]

        NoAlert ->
            text ""


closeAlertButton : Html Msg
closeAlertButton =
    button
        [ type_ "button", class "close", onClick DismissAlert ]
        [ span [] [ text "×" ] ]


openIssueUrl : String -> String
openIssueUrl error =
    crossOrigin "https://github.com"
        [ "RoboZonky", "natural-strategy-setup", "issues", "new" ]
        [ string "title" "Chyba při obnově strategie z URL"
        , string "body" error
        ]
