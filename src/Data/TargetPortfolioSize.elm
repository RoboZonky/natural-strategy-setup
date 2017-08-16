module Data.TargetPortfolioSize
    exposing
        ( TargetPortfolioSize(..)
        , renderTargetPortfolioSize
        )


type TargetPortfolioSize
    = Unbounded
    | Bounded Int


renderTargetPortfolioSize : TargetPortfolioSize -> String
renderTargetPortfolioSize targetPortfolioSize =
    case targetPortfolioSize of
        Bounded maxBound ->
            "Cílová zůstatková částka je " ++ toString maxBound ++ " Kč."

        Unbounded ->
            ""
