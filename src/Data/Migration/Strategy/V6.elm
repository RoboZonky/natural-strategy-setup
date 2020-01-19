module Data.Migration.Strategy.V6 exposing (fromV5)

import Data.Investment as Investment
import Data.Migration.Migration exposing (MigrationWarning)
import Data.Migration.Strategy.V1.Investment as V1Investment
import Data.Migration.Strategy.V5 as V5
import Data.Migration.Strategy.V5.InvestmentShare as V5IS
import Data.Strategy exposing (StrategyConfiguration)
import Dict.Any


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
            , defaultPrimaryInvestmentSize = newDefaultInvestmentSize

            -- duplicate inv size
            , defaultSecondaryPurchaseSize = newDefaultInvestmentSize
            , reservationSetting = oldGS.reservationSetting
            }

        diShareWarning =
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

        ( newDefaultInvestmentSize, diSizeWarning ) =
            migrateInvestment oldGS.defaultInvestmentSize

        ( newInvestmentSizeOverrides, ispWarning ) =
            migrateInvestmentsPerRating old.investmentSizeOverrides
    in
    ( { generalSettings = newGeneralSettings
      , portfolioStructure = old.portfolioStructure
      , primaryInvestmentOverrides = newInvestmentSizeOverrides

      -- duplicate inv sizes
      , secondaryPurchaseOverrides = newInvestmentSizeOverrides
      , buyingConfig = old.buyingConfig
      , sellingConfig = old.sellingConfig
      }
    , diShareWarning ++ diSizeWarning ++ ispWarning
    )


migrateInvestment : V1Investment.Size -> ( Investment.Size, List MigrationWarning )
migrateInvestment oldSize =
    ( migrateSize oldSize
    , [{- TODO warnings -}]
    )


migrateInvestmentsPerRating : V1Investment.InvestmentsPerRating -> ( Investment.InvestmentsPerRating, List MigrationWarning )
migrateInvestmentsPerRating oldIpr =
    ( Dict.Any.map (\_ range -> migrateSize range) oldIpr
    , [{- TODO warnings -}]
    )


migrateSize : V1Investment.Size -> Investment.Size
migrateSize =
    Investment.fromInt << Tuple.second << V1Investment.toIntRange
