module Data.Migration.Strategy.V6 exposing (fromV5)

import Data.Migration.Migration exposing (MigrationWarning)
import Data.Migration.Strategy.V5 as V5
import Data.Strategy exposing (StrategyConfiguration)


{-| V5 -> V6: TODO describe V6 changes
-}
fromV5 : V5.StrategyConfiguration -> ( StrategyConfiguration, List MigrationWarning )
fromV5 old =
    ( { generalSettings = old.generalSettings
      , portfolioStructure = old.portfolioStructure
      , investmentSizeOverrides = old.investmentSizeOverrides
      , buyingConfig = old.buyingConfig
      , sellingConfig = old.sellingConfig
      }
    , []
    )
