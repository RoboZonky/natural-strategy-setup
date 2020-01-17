module Data.Migration.Strategy.V6 exposing (fromV5)

import Data.Migration.Migration exposing (MigrationWarning)
import Data.Migration.Strategy.V5 as V5
import Data.Migration.Strategy.V5.InvestmentShare as V5IS
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
    let
        oldGS =
            old.generalSettings

        newGeneralSettings =
            { portfolio = oldGS.portfolio
            , exitConfig = oldGS.exitConfig
            , targetPortfolioSize = oldGS.targetPortfolioSize
            , defaultInvestmentSize = oldGS.defaultInvestmentSize
            , reservationSetting = oldGS.reservationSetting
            }

        disWarning =
            case oldGS.defaultInvestmentShare of
                V5IS.NotSpecified ->
                    []

                -- TODO is this error consistent with others
                -- TODO add test for this warning
                notDefault ->
                    [ "Vaše strategie měla nastaveno '"
                        ++ V5IS.render notDefault
                        ++ "'. Podpora pro toto nastavení býla v odstraněna v RoboZonky 5.7.0"
                    ]
    in
    ( { generalSettings = newGeneralSettings
      , portfolioStructure = old.portfolioStructure
      , investmentSizeOverrides = old.investmentSizeOverrides
      , buyingConfig = old.buyingConfig
      , sellingConfig = old.sellingConfig
      }
    , disWarning
    )
