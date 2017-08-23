module Data.InvestmentShare
    exposing
        ( InvestmentShare(..)
        , renderInvestmentShare
        )


type InvestmentShare
    = Unrestricted --in practice bounded by Zonky, which permits max 5000 Kč
    | PercentShare Int


renderInvestmentShare : InvestmentShare -> String
renderInvestmentShare investmentShare =
    case investmentShare of
        PercentShare share ->
            "Investovat maximálně " ++ toString share ++ " % výše úvěru."

        Unrestricted ->
            ""
