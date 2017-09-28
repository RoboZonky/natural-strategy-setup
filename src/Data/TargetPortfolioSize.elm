module Data.TargetPortfolioSize
    exposing
        ( TargetPortfolioSize(..)
        , renderTargetPortfolioSize
        , validate
        )

import Util


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


validate : TargetPortfolioSize -> List String
validate tb =
    case tb of
        NotSpecified ->
            []

        TargetPortfolioSize val ->
            Util.validate (val < 0) "Cílová zůstatková částka nesmí být záporná."
