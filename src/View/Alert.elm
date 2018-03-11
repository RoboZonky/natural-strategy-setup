module View.Alert exposing (view)

import Bootstrap.Alert
import Html exposing (Html, a, button, span, text)
import Html.Attributes exposing (class, href, type_)
import Html.Events exposing (onClick)
import Http
import Types exposing (AlertData(..), Msg(DismisAlert))


view : AlertData -> Html Msg
view maybeAlert =
    case maybeAlert of
        SuccessAlert successText ->
            Bootstrap.Alert.simpleSuccess []
                [ text successText
                , button [ type_ "button", class "close", onClick DismisAlert ] [ span [] [ text "×" ] ]
                ]

        ErrorAlert error ->
            Bootstrap.Alert.simpleDanger []
                [ text "Pokus o načtení strategie z URL se nezdařil. Prosím nahlašte chybu na "
                , a [ href (openIssueUrl error) ] [ text "stránce projektu." ]
                , button [ type_ "button", class "close", onClick DismisAlert ] [ span [] [ text "×" ] ]
                ]

        NoAlert ->
            text ""


openIssueUrl : String -> String
openIssueUrl error =
    "https://github.com/RoboZonky/natural-strategy-setup/issues/new?title="
        ++ Http.encodeUri "Chyba při obnově strategie z URL"
        ++ "&body="
        ++ Http.encodeUri error
