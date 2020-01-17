module Data.Migration.Strategy.V6 exposing (fromV5)

import Data.Migration.Migration exposing (MigrationWarning)
import Data.Migration.Strategy.V5 as V5
import Data.Strategy exposing (StrategyConfiguration)


{-| V5 -> V6:

  - GeneralSettings
      - added field `defaultPurchaseSize`
      - changed field `defaultInvestmentSize` from range to single value
      - removed field `defaultInvestmentShare : InvestmentShare`
  - StrategyConfiguration
      - added purchaseSizeOverrides
      - changed investmentSizeOverrides from range to single numbers
      - Buying and Selling configuration
        -- TODO new conditions added

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
