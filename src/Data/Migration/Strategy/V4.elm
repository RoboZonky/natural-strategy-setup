module Data.Migration.Strategy.V4 exposing
    ( GeneralSettings
    , StrategyConfiguration
    , fromV3
    , generalSettingsDecoder
    , strategyDecoder
    )

import Data.ExitConfig as ExitConfig exposing (ExitConfig)
import Data.Filter as Filters exposing (BuyingConfiguration, SellingConfiguration)
import Data.Filter.Conditions.Rating exposing (Rating)
import Data.Migration.Migration exposing (MigrationWarning)
import Data.Migration.Strategy.V1.Investment as Investment exposing (InvestmentsPerRating)
import Data.Migration.Strategy.V1.TargetBalance as TargetBalance
import Data.Migration.Strategy.V3 as V3
import Data.Migration.Strategy.V4.PortfolioStructure as PortfolioStructure
import Data.Migration.Strategy.V5.InvestmentShare as InvestmentShare exposing (InvestmentShare)
import Data.Portfolio as Portfolio exposing (Portfolio)
import Data.ReservationSetting as ReservationSetting exposing (ReservationSetting)
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize)
import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import RangeSlider exposing (RangeSlider)


type alias StrategyConfiguration =
    { generalSettings : GeneralSettings
    , portfolioShares : PortfolioShares
    , investmentSizeOverrides : InvestmentsPerRating
    , buyingConfig : BuyingConfiguration
    , sellingConfig : SellingConfiguration
    }


type alias GeneralSettings =
    { portfolio : Portfolio
    , exitConfig : ExitConfig
    , targetPortfolioSize : TargetPortfolioSize
    , defaultInvestmentSize : Investment.Size
    , defaultInvestmentShare : InvestmentShare
    , reservationSetting : ReservationSetting
    }


type alias PortfolioShares =
    AnyDict Int Rating Share


type alias Share =
    RangeSlider


{-| V3 -> V4: V4 removes support for TargetBalance
-}
fromV3 : V3.StrategyConfiguration -> ( StrategyConfiguration, List MigrationWarning )
fromV3 old =
    let
        shouldWarnAboutRemovedTargetBalance =
            old.generalSettings.defaultTargetBalance /= TargetBalance.NotSpecified

        perhapsWarning =
            if shouldWarnAboutRemovedTargetBalance then
                [ "strategie měla nastaveno omezení investic na základě disponibilního zůstatku\n\""
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


generalSettingsDecoder : Decoder GeneralSettings
generalSettingsDecoder =
    Decode.map6 GeneralSettings
        (Decode.field "a" Portfolio.decoder)
        (Decode.field "b" ExitConfig.decoder)
        (Decode.field "c" TargetPortfolioSize.decoder)
        (Decode.field "d" Investment.sizeDecoder)
        (Decode.field "e" InvestmentShare.decoder)
        (Decode.field "g1" ReservationSetting.decoder)
