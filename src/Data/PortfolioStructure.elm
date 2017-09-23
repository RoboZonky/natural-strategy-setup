module Data.PortfolioStructure
    exposing
        ( PortfolioShare
        , PortfolioShares
        , Share
        , renderPortfolioShare
        , renderPortfolioShares
        )

import AllDict exposing (AllDict)
import Data.Filter.Conditions.Rating as Rating exposing (Rating(..))
import Data.Portfolio exposing (Portfolio(Empty))
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
            AllDict.toList shares
                -- Only render share in the config when maximum > 0
                |> List.filter (\( _, ( _, mx ) ) -> mx > 0)
                |> List.map renderPortfolioShare
                |> Util.renderNonemptySection "\n- Úprava struktury portfolia"

        _ ->
            ""
