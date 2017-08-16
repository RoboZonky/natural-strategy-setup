module Data.InvestmentShare
    exposing
        ( InvestmentShare(..)
        , renderInvestmentShare
        )


type InvestmentShare
    = Unbounded --in practice bounded by Zonky, which permits max 5000 Kč
    | InvestmentShare Int


renderInvestmentShare : InvestmentShare -> String
renderInvestmentShare investmentShare =
    case investmentShare of
        InvestmentShare share ->
            "Investovat maximálně " ++ toString share ++ " % výše úvěru."

        Unbounded ->
            ""
