module Data.TargetPortfolioSize
    exposing
        ( TargetPortfolioSize(..)
        , renderTargetPortfolioSize
        )


type TargetPortfolioSize
    = NotSpecified
    | TargetPortfolioSize Int


renderTargetPortfolioSize : TargetPortfolioSize -> String
renderTargetPortfolioSize targetPortfolioSize =
    case targetPortfolioSize of
        TargetPortfolioSize maxBound ->
            "Cílová zůstatková částka je " ++ toString maxBound ++ " Kč."

        NotSpecified ->
            ""
