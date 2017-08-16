module Data.InvestmentSize
    exposing
        ( InvestmentSize(..)
        , renderInvestmentSizeDefault
        , renderInvestmentSizeOverride
        )

import Data.Rating exposing (Rating, ratingToString)


type InvestmentSize
    = Amount Int
    | UpTo Int
    | FromTo Int Int


renderInvestmentSizeDefault : InvestmentSize -> String
renderInvestmentSizeDefault investmentSize =
    "Běžná výše investice je" ++ renderInvestmentSize investmentSize ++ " Kč."


renderInvestmentSizeOverride : Rating -> InvestmentSize -> String
renderInvestmentSizeOverride rating investmentSize =
    "Do úvěrů v ratingu " ++ ratingToString rating ++ " investovat " ++ renderInvestmentSize investmentSize


renderInvestmentSize : InvestmentSize -> String
renderInvestmentSize investmentSize =
    case investmentSize of
        Amount amt ->
            toString amt

        UpTo amt ->
            " až " ++ toString amt

        FromTo from to ->
            toString from ++ " až " ++ toString to
