module Data.Investment
    exposing
        ( InvestmentPerRating(..)
        , Size(..)
        , renderDefaultInvestmentSize
        , renderInvestment
        , renderInvestments
        )

import AllDict as Dict exposing (AllDict)
import Data.Rating exposing (Rating, ratingToString)
import Util


type InvestmentPerRating
    = InvestmentPerRating Rating Size


type alias InvestmentsPerRating =
    AllDict Rating Size Int


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
