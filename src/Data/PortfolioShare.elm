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
            Dict.toList shares
                -- Only render share in the config when maximum > 0
                |> List.filter (\( _, ( _, mx ) ) -> mx > 0)
                |> List.map renderPortfolioShare
                |> Util.renderNonemptySection "\n- Úprava struktury portfolia"

        _ ->
            ""
