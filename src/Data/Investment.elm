module Data.Investment
    exposing
        ( InvestmentPerRating(..)
        , Size(..)
        , renderDefaultInvestmentSize
        , renderInvestments
        , renderInvestment
        )

import Util
import Data.Rating exposing (Rating, ratingToString)


type InvestmentPerRating
    = InvestmentPerRating Rating Size


type Size
    = Amount Int
    | UpTo Int
    | FromTo Int Int


renderDefaultInvestmentSize : Size -> String
renderDefaultInvestmentSize investmentSize =
    "Běžná výše investice je" ++ investmentSizeToString investmentSize ++ " Kč."


renderInvestment : InvestmentPerRating -> String
renderInvestment (InvestmentPerRating rating investmentSize) =
    "Do úvěrů v ratingu " ++ ratingToString rating ++ " investovat" ++ investmentSizeToString investmentSize ++ " Kč."


renderInvestments : List InvestmentPerRating -> String
renderInvestments investments =
    Util.renderNonemptySection "\n- Výše investice" <|
        List.map renderInvestment investments


investmentSizeToString : Size -> String
investmentSizeToString investmentSize =
    case investmentSize of
        Amount amt ->
            " " ++ toString amt

        UpTo amt ->
            " až " ++ toString amt

        FromTo from to ->
            " " ++ toString from ++ " až " ++ toString to
