module View.Alert exposing
    ( AlertData(..)
    , view
    )

import Bootstrap.Alert
import Html exposing (Html)
import Html.Attributes exposing (class, href, target, type_)
import Html.Events exposing (onClick)
import Url.Builder exposing (crossOrigin, string)


type AlertData
    = SuccessAlert String
    | WarningAlert (List String)
    | ErrorAlert String
    | NoAlert


view : msg -> AlertData -> Html msg
view dismissAlert maybeAlert =
    case maybeAlert of
        SuccessAlert successText ->
            Bootstrap.Alert.simpleSuccess []
                [ Html.text successText
                , closeAlertButton dismissAlert
                ]

        ErrorAlert error ->
            Bootstrap.Alert.simpleDanger []
                [ Html.text "Pokus o načtení strategie z URL se nezdařil. Prosím nahlašte chybu na stránce projektu kliknutím na tento "
                , Html.a [ href (openIssueUrl error) ] [ Html.text "odkaz" ]
                , closeAlertButton dismissAlert
                ]

        WarningAlert warnings ->
            Bootstrap.Alert.simpleWarning []
                [ closeAlertButton dismissAlert
                , Html.div [] [ Html.text "Upozornění: Strategii se podařilo obnovit z URL jen částečně." ]
                , Html.div []
                    [ Html.text "Došlo k několika zpětně nekompatibilním změnám ve formátu strategie (viz "
                    , Html.a
                        [ href "https://github.com/RoboZonky/natural-strategy-setup/blob/master/docs/StrategyFormatChangelog.md"
                        , target "_blank"
                        ]
                        [ Html.text "dokumentace" ]
                    , Html.text ")"
                    ]
                , Html.div [] [ Html.text "V důsledku toho musely být některé části vaší konfigurace odstraněny." ]
                , Html.ul [] <| List.map (\warning -> Html.li [] [ Html.text warning ]) warnings
                ]

        NoAlert ->
            Html.text ""


closeAlertButton : msg -> Html msg
closeAlertButton dismissAlert =
    Html.button
        [ type_ "button", class "close", onClick dismissAlert ]
        [ Html.span [] [ Html.text "×" ] ]


openIssueUrl : String -> String
openIssueUrl error =
    crossOrigin "https://github.com"
        [ "RoboZonky", "natural-strategy-setup", "issues", "new" ]
        [ string "title" "Chyba při obnově strategie z URL"
        , string "body" error
        ]
