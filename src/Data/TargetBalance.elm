module Data.TargetBalance
    exposing
        ( TargetBalance(..)
        , defaultTargetBalance
        , renderTargetBalance
        )


type TargetBalance
    = Unspecified
    | TargetBalance Int


defaultTargetBalance : TargetBalance
defaultTargetBalance =
    TargetBalance 200


renderTargetBalance : TargetBalance -> String
renderTargetBalance targetBalance =
    case targetBalance of
        TargetBalance balance ->
            if balance /= 200 then
                "Investovat pouze pokud disponibilní zůstatek přesáhne " ++ toString balance ++ " Kč."
            else
                ""

        Unspecified ->
            ""
