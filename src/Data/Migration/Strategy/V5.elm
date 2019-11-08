module Data.Migration.Strategy.V5 exposing (fromV4)

import Data.Filter.Conditions.Rating as Rating
import Data.Migration.Migration exposing (MigrationWarning)
import Data.Migration.Strategy.V4 as V4
import Data.Migration.Strategy.V4.PortfolioStructure as V4PS
import Data.PortfolioStructure as V5PS
import Data.Strategy exposing (StrategyConfiguration)
import Dict.Any
import Percentage


{-| V4 -> V5: V5 changes PortfolioShares - they're no longer defined in terms of ranges (min - max).
Instead each rating has just one associated value (corresponding to what used to be "max")
-}
fromV4 : V4.StrategyConfiguration -> ( StrategyConfiguration, List MigrationWarning )
fromV4 old =
    let
        ( newPortfolioShares, warnings ) =
            migratePortfolioStructure old.portfolioShares
    in
    ( { generalSettings = old.generalSettings
      , portfolioShares = newPortfolioShares
      , investmentSizeOverrides = old.investmentSizeOverrides
      , buyingConfig = old.buyingConfig
      , sellingConfig = old.sellingConfig
      }
    , warnings
    )


migratePortfolioStructure : V4PS.PortfolioShares -> ( V5PS.PortfolioShares, List MigrationWarning )
migratePortfolioStructure v4shares =
    let
        sharesDefinedWithRanges =
            v4shares
                |> Dict.Any.map (\_ slider -> V4PS.toIntRange slider)
                -- Warning will be shown if user had minimum /= maximum for any rating
                |> Dict.Any.filter (\_ ( min, max ) -> min /= max)
                |> Dict.Any.toList

        simplificationWarning =
            if List.isEmpty sharesDefinedWithRanges then
                []

            else
                sharesDefinedWithRanges
                    |> List.map
                        (\( rating, ( min, max ) ) ->
                            " • "
                                ++ Rating.showInterestPercent rating
                                ++ " z rozsahu '"
                                ++ String.fromInt min
                                ++ " až "
                                ++ String.fromInt max
                                ++ "%' na '"
                                ++ String.fromInt max
                                ++ "%'"
                        )
                    |> String.join "\n"
                    |> (\items -> "Vaše strategie měla nastavenu vámi definovanou strukturu portfolia, která musela být zjednodušena:\n" ++ items)
                    |> List.singleton

        maxima =
            Dict.Any.values v4shares
                |> List.map (Percentage.fromInt << Tuple.second << V4PS.toIntRange)
    in
    case V5PS.fromPercentageList maxima of
        Ok v5shares ->
            ( v5shares, simplificationWarning )

        Err errorAboutIncorrectStructure ->
            -- This shouldn't happen, unless people manually meddle with the URL encoded strategy
            ( V5PS.conservative
            , [ errorAboutIncorrectStructure
                    ++ " \nNastavil jsem 'konzervativní' portfolio, které si můžete v sekci 'Struktura portfolia' upravit podle potřeby."
              ]
            )
