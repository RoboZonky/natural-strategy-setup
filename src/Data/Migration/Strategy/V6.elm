module Data.Migration.Strategy.V6 exposing (fromV5, investmentSizeWarning)

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
  - Selling configuration
      - New option "Prodávat všechny participace bez poplatku a slevy, které odpovídají filtrům tržiště."
  - New conditions
      - Health (for Participations)
      - Original Term Moths (for Participations)
      - Relative Sale Discount (for Participations)
      - Relative profit (for Participations to sell)

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

                notDefault ->
                    [ "strategie obsahovala nastavení '"
                        ++ V5IS.render notDefault
                        ++ "', které muselo být odstraněno."
                    ]

        newDefaultInvestmentSize =
            migrateInvestment oldGS.defaultInvestmentSize

        newInvestmentSizeOverrides =
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
    , investmentSizeWarning :: diShareWarning
    )


investmentSizeWarning : String
investmentSizeWarning =
    "došlo k rozdělení konfigurace výše investic pro primární a sekundárni tržiště."
        ++ " Důrazně doporučujeme přečíst si o změně v dokumentaci a toto nastavení si překontrolovat."


migrateInvestment : V1Investment.Size -> Investment.Size
migrateInvestment oldSize =
    migrateSize oldSize


migrateInvestmentsPerRating : V1Investment.InvestmentsPerRating -> Investment.InvestmentsPerRating
migrateInvestmentsPerRating oldIpr =
    Dict.Any.map (\_ range -> migrateSize range) oldIpr


migrateSize : V1Investment.Size -> Investment.Size
migrateSize =
    Investment.fromInt << Tuple.second << V1Investment.toIntRange
