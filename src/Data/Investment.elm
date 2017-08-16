module Data.Investment
    exposing
        ( InvestmentPerRating(..)
        , Size(..)
        , renderDefaultInvestmentSize
        , renderInvestments
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
    "Do úvěrů v ratingu " ++ ratingToString rating ++ " investovat " ++ investmentSizeToString investmentSize


renderInvestments : List InvestmentPerRating -> String
renderInvestments investments =
    case investments of
        [] ->
            ""

        nonempty ->
            Util.joinNonemptyLines <|
                "\n- Výše investice"
                    :: List.map renderInvestment nonempty


investmentSizeToString : Size -> String
investmentSizeToString investmentSize =
    case investmentSize of
        Amount amt ->
            " " ++ toString amt

        UpTo amt ->
            " až " ++ toString amt

        FromTo from to ->
            toString from ++ " až " ++ toString to
