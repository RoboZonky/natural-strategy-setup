module Data.Migration.Strategy.V3 exposing
    ( GeneralSettings
    , StrategyConfiguration
    , fromV2
    , strategyDecoder
    )

import Data.Confirmation as Confirmation
import Data.ExitConfig as ExitConfig exposing (ExitConfig)
import Data.Filter as Filters exposing (BuyingConfiguration, SellingConfiguration)
import Data.Investment as Investment exposing (InvestmentsPerRating)
import Data.InvestmentShare as InvestmentShare exposing (InvestmentShare)
import Data.Migration.Migration exposing (MigrationWarning)
import Data.Migration.Strategy.V2 as V2
import Data.Migration.Strategy.V4.PortfolioStructure as PortfolioStructure exposing (PortfolioShares)
import Data.Portfolio as Portfolio exposing (Portfolio(..))
import Data.ReservationSetting as ReservationSetting exposing (ReservationSetting)
import Data.TargetBalance as TargetBalance exposing (TargetBalance)
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize)
import Json.Decode as Decode exposing (Decoder)


type alias GeneralSettings =
    { portfolio : Portfolio
    , exitConfig : ExitConfig
    , targetPortfolioSize : TargetPortfolioSize
    , defaultInvestmentSize : Investment.Size
    , defaultInvestmentShare : InvestmentShare
    , defaultTargetBalance : TargetBalance
    , reservationSetting : ReservationSetting
    }


type alias StrategyConfiguration =
    { generalSettings : GeneralSettings
    , portfolioShares : PortfolioShares
    , investmentSizeOverrides : InvestmentsPerRating
    , buyingConfig : BuyingConfiguration
    , sellingConfig : SellingConfiguration
    }


generalSettingsDecoder : Decoder GeneralSettings
generalSettingsDecoder =
    Decode.map7 GeneralSettings
        (Decode.field "a" Portfolio.decoder)
        (Decode.field "b" ExitConfig.decoder)
        (Decode.field "c" TargetPortfolioSize.decoder)
        (Decode.field "d" Investment.sizeDecoder)
        (Decode.field "e" InvestmentShare.decoder)
        (Decode.field "f" TargetBalance.decoder)
        (Decode.field "g1" ReservationSetting.decoder)


strategyDecoder : Decoder StrategyConfiguration
strategyDecoder =
    {- Need the portfolio ahead of time because it determines if we
       should decode portfolio structure (for UserDefined) or use one of the predefined ones
    -}
    Decode.field "h" generalSettingsDecoder
        |> Decode.andThen
            (\generalSettings ->
                Decode.map4 (StrategyConfiguration generalSettings)
                    (PortfolioStructure.decoderFromPortfolio generalSettings.portfolio)
                    (Decode.field "j" Investment.decoder)
                    (Decode.field "k" Filters.decodeBuyingConfiguration)
                    (Decode.field "l" Filters.decodeSellingConfiguration)
            )


{-| V2 -> V3: V3 removes the ConfirmationSettings condition
-}
fromV2 : V2.StrategyConfiguration -> ( StrategyConfiguration, List MigrationWarning )
fromV2 old =
    let
        shouldWarnAboutRemovedConfirmation =
            old.generalSettings.confirmationSettings /= Confirmation.defaultSettings

        perhapsWarning =
            if shouldWarnAboutRemovedConfirmation then
                [ "strategie měla nastaveno Potvrzení investic mobilem, které muselo být odstraněno." ]

            else
                []
    in
    ( removeConfirmationSettings old, perhapsWarning )


removeConfirmationSettings : V2.StrategyConfiguration -> StrategyConfiguration
removeConfirmationSettings old =
    let
        removeConfirmationSettings_ : V2.GeneralSettings -> GeneralSettings
        removeConfirmationSettings_ gs =
            { portfolio = gs.portfolio
            , exitConfig = gs.exitConfig
            , targetPortfolioSize = gs.targetPortfolioSize
            , defaultInvestmentSize = gs.defaultInvestmentSize
            , defaultInvestmentShare = gs.defaultInvestmentShare
            , defaultTargetBalance = gs.defaultTargetBalance
            , reservationSetting = gs.reservationSetting
            }
    in
    { generalSettings = removeConfirmationSettings_ old.generalSettings
    , portfolioShares = old.portfolioShares
    , investmentSizeOverrides = old.investmentSizeOverrides
    , buyingConfig = old.buyingConfig
    , sellingConfig = old.sellingConfig
    }
