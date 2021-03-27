module Data.Migration.Strategy.V2 exposing
    ( GeneralSettings
    , StrategyConfiguration
    , fromV1
    , strategyDecoder
    )

import Data.ExitConfig as ExitConfig exposing (ExitConfig)
import Data.Filter as Filters exposing (BuyingConfiguration, SellingConfiguration)
import Data.Filter.Conditions.Rating as Rating
import Data.Migration.Migration exposing (MigrationWarning)
import Data.Migration.Strategy.V1 as V1
import Data.Migration.Strategy.V1.Investment as Investment exposing (InvestmentsPerRating)
import Data.Migration.Strategy.V1.TargetBalance as TargetBalance exposing (TargetBalance)
import Data.Migration.Strategy.V2.Confirmation as Confirmation exposing (ConfirmationSettings)
import Data.Migration.Strategy.V4.PortfolioStructure as PortfolioStructure exposing (PortfolioShares)
import Data.Migration.Strategy.V5.InvestmentShare as InvestmentShare exposing (InvestmentShare)
import Data.Portfolio as Portfolio exposing (Portfolio)
import Data.ReservationSetting as ReservationSetting exposing (ReservationSetting)
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize)
import Json.Decode as Decode exposing (Decoder)


type alias GeneralSettings =
    { portfolio : Portfolio
    , exitConfig : ExitConfig
    , targetPortfolioSize : TargetPortfolioSize
    , defaultInvestmentSize : Investment.Size
    , defaultInvestmentShare : InvestmentShare
    , defaultTargetBalance : TargetBalance

    -- V3 removes ConfirmationSettings condition
    , confirmationSettings : ConfirmationSettings
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
    Decode.map8 GeneralSettings
        (Decode.field "a" Portfolio.decoder)
        (Decode.field "b" ExitConfig.decoder)
        (Decode.field "c" TargetPortfolioSize.decoder)
        (Decode.field "d" Investment.sizeDecoder)
        (Decode.field "e" InvestmentShare.decoder)
        (Decode.field "f" TargetBalance.decoder)
        (Decode.field "g" Confirmation.decoder)
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


{-| V1 -> V2 :

  - Changed mobile notification condition to be based on rating percent (instead of rating letters)
  - Removed "Loan Annuity", "Sale Fee" and "Revenue Rate" conditions

-}
fromV1 : V1.DecodedStrategy -> ( StrategyConfiguration, List MigrationWarning )
fromV1 { strategyConfig, removedBuyFilterCount, removedSellFilterCount } =
    let
        v2Strategy =
            removeLegacyConfirmation strategyConfig

        shouldWarnAboutRemovedConfirmation =
            strategyConfig.generalSettings.confirmationSettings /= Rating.defaultCondition

        removedConfirmationWarning =
            if shouldWarnAboutRemovedConfirmation then
                [ "strategie měla nastaveno Potvrzení investic mobilem, které muselo být odstraněno." ]

            else
                []

        removedBuyFiltersWarning =
            if removedBuyFilterCount > 0 then
                [ pluralizeRules removedBuyFilterCount ++ " nákupu odstraněno, protože obsahovaly zpětně nekompatibilní podmínky" ]

            else
                []

        removedSellFiltersWarning =
            if removedSellFilterCount > 0 then
                [ pluralizeRules removedSellFilterCount ++ " prodeje odstraněno, protože obsahovaly zpětně nekompatibilní podmínky" ]

            else
                []
    in
    ( v2Strategy
    , removedConfirmationWarning ++ removedBuyFiltersWarning ++ removedSellFiltersWarning
    )


removeLegacyConfirmation : V1.StrategyConfiguration -> StrategyConfiguration
removeLegacyConfirmation old =
    let
        removeLegacyConfirmation_ : V1.GeneralSettings -> GeneralSettings
        removeLegacyConfirmation_ gs =
            { portfolio = gs.portfolio
            , exitConfig = gs.exitConfig
            , targetPortfolioSize = gs.targetPortfolioSize
            , defaultInvestmentSize = gs.defaultInvestmentSize
            , defaultInvestmentShare = gs.defaultInvestmentShare
            , defaultTargetBalance = gs.defaultTargetBalance
            , confirmationSettings = Confirmation.defaultSettings
            , reservationSetting = ReservationSetting.defaultSetting
            }
    in
    { generalSettings = removeLegacyConfirmation_ old.generalSettings
    , portfolioShares = old.portfolioShares
    , investmentSizeOverrides = old.investmentSizeOverrides
    , buyingConfig = old.buyingConfig
    , sellingConfig = old.sellingConfig
    }


pluralizeRules : Int -> String
pluralizeRules x =
    String.fromInt x
        ++ (if x == 1 then
                " pravidlo"

            else if 2 <= x && x <= 4 then
                " pravidla"

            else
                " pravidel"
           )
