module Data.Migration.Strategy.V4 exposing
    ( StrategyConfiguration
    , fromV3
    , strategyDecoder
    )

import Data.Filter as Filters exposing (BuyingConfiguration, SellingConfiguration)
import Data.Filter.Conditions.Rating exposing (Rating)
import Data.Investment as Investment exposing (InvestmentsPerRating)
import Data.Migration.Migration exposing (MigrationWarning)
import Data.Migration.Strategy.V3 as V3
import Data.Migration.Strategy.V4.PortfolioStructure as PortfolioStructure
import Data.Portfolio exposing (Portfolio(..))
import Data.PortfolioStructure.PredefinedShares as PredefinedShares
import Data.Strategy exposing (GeneralSettings, StrategyConfiguration, generalSettingsDecoder)
import Data.TargetBalance as TargetBalance
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


strategyDecoder : Decoder StrategyConfiguration
strategyDecoder =
    {- Need the portfolio ahead of time because it determines if we
       should decode portfolio structure (for UserDefined) or use one of the predefined ones
    -}
    Decode.field "h" generalSettingsDecoder
        |> Decode.andThen
            (\generalSettings ->
                Decode.map4 (StrategyConfiguration generalSettings)
                    (portfolioStructureDecoder generalSettings.portfolio)
                    (Decode.field "j" Investment.decoder)
                    (Decode.field "k" Filters.decodeBuyingConfiguration)
                    (Decode.field "l" Filters.decodeSellingConfiguration)
            )


portfolioStructureDecoder : Portfolio -> Decoder PortfolioShares
portfolioStructureDecoder portfolio =
    case portfolio of
        Conservative ->
            Decode.succeed PredefinedShares.conservative

        Balanced ->
            Decode.succeed PredefinedShares.balanced

        Progressive ->
            Decode.succeed PredefinedShares.progressive

        UserDefined ->
            Decode.field "i" PortfolioStructure.decoder
