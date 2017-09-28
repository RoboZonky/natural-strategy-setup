module Data.TargetBalance
    exposing
        ( TargetBalance(..)
        , defaultTargetBalance
        , renderTargetBalance
        , validate
        )

import Util


type TargetBalance
    = NotSpecified
    | TargetBalance Int


defaultTargetBalance : TargetBalance
defaultTargetBalance =
    TargetBalance 200


renderTargetBalance : TargetBalance -> String
renderTargetBalance targetBalance =
    case targetBalance of
        TargetBalance balance ->
            "Investovat pouze pokud disponibilní zůstatek přesáhne " ++ toString balance ++ " Kč."

        NotSpecified ->
            ""


validate : TargetBalance -> List String
validate tb =
    case tb of
        NotSpecified ->
            []

        TargetBalance val ->
            Util.validate (val < 200) "Minimální výše investice na Zonky.cz je 200 Kč. Nastovavat nižší hodnotu nemá smysl."
