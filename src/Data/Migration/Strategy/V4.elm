module Data.Migration.Strategy.V4 exposing (fromV3)

import Data.Migration.Migration exposing (MigrationWarning)
import Data.Migration.Strategy.V3 as V3
import Data.Strategy exposing (GeneralSettings, StrategyConfiguration)
import Data.TargetBalance as TargetBalance


{-| V3 -> V4: V4 removes support for TargetBalance
-}
fromV3 : V3.StrategyConfiguration -> ( StrategyConfiguration, List MigrationWarning )
fromV3 old =
    let
        shouldWarnAboutRemovedTargetBalance =
            old.generalSettings.defaultTargetBalance /= TargetBalance.NotSpecified

        perhapsWarning =
            if shouldWarnAboutRemovedTargetBalance then
                [ "Vaše strategie měla nastaveno omezení investic na základě disponibilního zůstatku\n\""
                    ++ TargetBalance.render old.generalSettings.defaultTargetBalance
                    ++ "\"\n, které muselo být odstraněno."
                ]

            else
                []
    in
    ( removeTargetBalance old, perhapsWarning )


removeTargetBalance : V3.StrategyConfiguration -> StrategyConfiguration
removeTargetBalance old =
    let
        removeTargetBalance_ : V3.GeneralSettings -> GeneralSettings
        removeTargetBalance_ gs =
            { portfolio = gs.portfolio
            , exitConfig = gs.exitConfig
            , targetPortfolioSize = gs.targetPortfolioSize
            , defaultInvestmentSize = gs.defaultInvestmentSize
            , defaultInvestmentShare = gs.defaultInvestmentShare
            , reservationSetting = gs.reservationSetting
            }
    in
    { generalSettings = removeTargetBalance_ old.generalSettings
    , portfolioShares = old.portfolioShares
    , investmentSizeOverrides = old.investmentSizeOverrides
    , buyingConfig = old.buyingConfig
    , sellingConfig = old.sellingConfig
    }
