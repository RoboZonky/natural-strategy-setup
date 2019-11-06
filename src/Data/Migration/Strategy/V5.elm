module Data.Migration.Strategy.V5 exposing (fromV4)

import Data.Migration.Migration exposing (MigrationWarning)
import Data.Migration.Strategy.V4 as V4
import Data.Migration.Strategy.V4.PortfolioStructure as V4PS
import Data.PortfolioStructure as V5PS
import Data.Strategy exposing (StrategyConfiguration)


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
migratePortfolioStructure =
    Debug.todo ""
