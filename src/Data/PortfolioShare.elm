module Data.PortfolioShare
    exposing
        ( PortfolioShare(..)
        , Share(..)
        , renderPortfolioShare
        , renderPortfolioShares
        )

import Data.Rating as Rating exposing (Rating)
import Util


type PortfolioShare
    = PortfolioShare Rating Share


type Share
    = Exact Int
    | Range Int Int


renderPortfolioShare : PortfolioShare -> String
renderPortfolioShare (PortfolioShare rating share) =
    "Prostředky v ratingu " ++ Rating.ratingToString rating ++ " tvoří " ++ renderShare share ++ " % aktuální zůstatkové částky."


renderShare : Share -> String
renderShare share =
    case share of
        Exact percentage ->
            toString percentage

        Range minPercent maxPercent ->
            toString minPercent ++ " až " ++ toString maxPercent


renderPortfolioShares : List PortfolioShare -> String
renderPortfolioShares shares =
    Util.renderNonemptySection "\n- Úprava struktury portfolia" <|
        List.map renderPortfolioShare shares
