module Data.TargetBalance
    exposing
        ( TargetBalance(..)
        , renderTargetBalance
        )


type TargetBalance
    = Unspecified
    | TargetBalance Int


renderTargetBalance : TargetBalance -> String
renderTargetBalance targetBalance =
    case targetBalance of
        TargetBalance balance ->
            "Investovat pouze pokud disponibilní zůstatek přesáhne " ++ toString balance ++ " Kč."

        Unspecified ->
            ""
