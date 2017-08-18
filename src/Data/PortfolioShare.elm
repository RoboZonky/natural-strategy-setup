module Data.PortfolioShare
    exposing
        ( PortfolioShares
        , PortfolioShare
        , Share
        , renderPortfolioShare
        , renderPortfolioShares
        )

import Data.Portfolio exposing (Portfolio(Empty))
import Data.Rating as Rating exposing (Rating(..))
import AllDict as Dict exposing (AllDict)
import Util


type alias PortfolioShares =
    AllDict Rating ( Int, Int ) Int


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


renderPortfolioShares : Portfolio -> PortfolioShares -> String
renderPortfolioShares portfolio shares =
    case portfolio of
        -- Only render this section when user "overrides" predefined DefaultPortfolios
        Empty ->
            Util.renderNonemptySection "\n- Úprava struktury portfolia" <|
                List.map renderPortfolioShare <|
                    Dict.toList shares

        _ ->
            ""
