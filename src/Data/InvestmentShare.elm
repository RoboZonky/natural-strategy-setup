module Data.InvestmentShare
    exposing
        ( InvestmentShare(..)
        , renderInvestmentShare
        )


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
