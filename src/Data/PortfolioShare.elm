module Data.PortfolioShare
    exposing
        ( PortfolioShares
        , PortfolioShare
        , Share
        , renderPortfolioShare
        , renderPortfolioShares
        )

import Data.Rating as Rating exposing (Rating)
import Util
import EveryDict as Dict exposing (EveryDict)
import Data.Rating exposing (Rating(..))


type alias PortfolioShares =
    EveryDict Rating ( Int, Int )


type alias PortfolioShare =
    ( Rating, Share )


type alias Share =
    ( Int, Int )


renderPortfolioShare : PortfolioShare -> String
renderPortfolioShare ( rating, share ) =
    "Prostředky v ratingu " ++ Rating.ratingToString rating ++ " tvoří " ++ renderShare share ++ " % aktuální zůstatkové částky."


renderShare : Share -> String
renderShare ( minPercent, maxPercent ) =
    if minPercent == maxPercent then
        toString minPercent
    else
        toString minPercent ++ " až " ++ toString maxPercent


renderPortfolioShares : PortfolioShares -> String
renderPortfolioShares shares =
    Util.renderNonemptySection "\n- Úprava struktury portfolia" <|
        List.map renderPortfolioShare <|
            Dict.toList shares
