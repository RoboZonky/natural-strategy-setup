module Data.InvestmentShare
    exposing
        ( InvestmentShare(..)
        , renderInvestmentShare
        , validate
        )

import Util


type InvestmentShare
    = NotSpecified --in practice bounded by Zonky, which permits max 5000 Kč
    | InvestmentSharePercent Int


renderInvestmentShare : InvestmentShare -> String
renderInvestmentShare investmentShare =
    case investmentShare of
        InvestmentSharePercent share ->
            "Investovat maximálně " ++ toString share ++ " % výše úvěru."

        NotSpecified ->
            ""


validate : InvestmentShare -> List String
validate s =
    case s of
        NotSpecified ->
            []

        InvestmentSharePercent pct ->
            Util.validate (pct < 1 || 100 < pct) "Podíl výše úvěru musí být mezi 1 a 100 %"
