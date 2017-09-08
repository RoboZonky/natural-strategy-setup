module Data.Investment
    exposing
        ( InvestmentsPerRating
        , Size
        , defaultInvestmentsPerRating
        , defaultSize
        , renderDefaultInvestmentSize
        , renderInvestment
        , renderInvestments
        )

import AllDict exposing (AllDict)
import Data.Filter.Condition.Rating as Rating exposing (Rating, ratingToString)
import Util


type InvestmentPerRating
    = InvestmentPerRating Rating Size


type alias InvestmentsPerRating =
    AllDict Rating Size Int


defaultInvestmentsPerRating : Size -> InvestmentsPerRating
defaultInvestmentsPerRating defaultSize =
    AllDict.fromList Rating.hash <| List.map (\r -> ( r, defaultSize )) Rating.allRatings


type alias Size =
    ( Int, Int )


defaultSize : Size
defaultSize =
    ( 200, 200 )


renderDefaultInvestmentSize : Size -> String
renderDefaultInvestmentSize investmentSize =
    if investmentSize /= defaultSize then
        "Běžná výše investice je" ++ investmentSizeToString investmentSize ++ " Kč."
    else
        ""


renderInvestment : InvestmentPerRating -> String
renderInvestment (InvestmentPerRating rating investmentSize) =
    "Do úvěrů v ratingu " ++ ratingToString rating ++ " investovat" ++ investmentSizeToString investmentSize ++ " Kč."


renderInvestments : Size -> InvestmentsPerRating -> String
renderInvestments defaultSize investments =
    if AllDict.isEmpty investments then
        ""
    else
        AllDict.toList investments
            --filter our sizes equal to default size
            |> List.filter (\( _, invSize ) -> invSize /= defaultSize)
            |> List.map (\( rating, invSize ) -> InvestmentPerRating rating invSize)
            |> List.map renderInvestment
            |> Util.renderNonemptySection "\n- Výše investice"


investmentSizeToString : Size -> String
investmentSizeToString ( mini, maxi ) =
    if mini == maxi then
        " " ++ toString mini
    else if mini == 0 then
        " až " ++ toString maxi
    else
        " " ++ toString mini ++ " až " ++ toString maxi
