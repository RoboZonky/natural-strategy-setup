module Data.Strategy exposing
    ( GeneralSettings
    , StrategyConfiguration
    , addBuyFilter
    , addSellFilter
    , defaultStrategyConfiguration
    , removeBuyFilter
    , removeSellFilter
    , renderStrategyConfiguration
    , setBuyingConfiguration
    , setDefaultInvestment
    , setDefaultInvestmentShare
    , setExitConfig
    , setInvestment
    , setPortfolio
    , setPortfolioShareRange
    , setReservationSetting
    , setSellingConfiguration
    , setTargetBalance
    , setTargetPortfolioSize
    , strategyDecoder
    , strategyEqual
    , strategyToUrlHash
    , togglePrimaryMarket
    , toggleSecondaryMarket
    , updateNotificationSettings
    , validateStrategyConfiguration
    )

import Base64
import Data.Confirmation as Confirmation exposing (ConfirmationFormMsg, ConfirmationSettings)
import Data.ExitConfig as ExitConfig exposing (ExitConfig)
import Data.Filter as Filters exposing (BuyingConfiguration, MarketplaceFilter, SellingConfiguration)
import Data.Filter.Conditions.Rating exposing (Rating(..))
import Data.Investment as Investment exposing (InvestmentsPerRating)
import Data.InvestmentShare as InvestmentShare exposing (InvestmentShare)
import Data.Portfolio as Portfolio exposing (Portfolio(..))
import Data.PortfolioStructure as PortfolioStructure exposing (PortfolioShares)
import Data.PortfolioStructure.PredefinedShares as PredefinedShares
import Data.ReservationSetting as ReservationSetting exposing (ReservationSetting)
import Data.TargetBalance as TargetBalance exposing (TargetBalance)
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize)
import Dict.Any
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra
import RangeSlider
import Time exposing (Posix)
import Types exposing (BaseUrl, UrlHash)
import Util
import Version


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
    , defaultTargetBalance : TargetBalance
    , confirmationSettings : ConfirmationSettings
    , reservationSetting : ReservationSetting
    }


defaultStrategyConfiguration : StrategyConfiguration
defaultStrategyConfiguration =
    { generalSettings =
        { portfolio = Portfolio.Conservative
        , exitConfig = ExitConfig.DontExit
        , targetPortfolioSize = TargetPortfolioSize.NotSpecified
        , defaultInvestmentSize = Investment.defaultSize
        , defaultInvestmentShare = InvestmentShare.NotSpecified
        , defaultTargetBalance = TargetBalance.NotSpecified
        , confirmationSettings = Confirmation.confirmationsDisabled
        , reservationSetting = ReservationSetting.Ignore
        }
    , portfolioShares = PredefinedShares.conservative
    , investmentSizeOverrides = Investment.defaultInvestmentsPerRating Investment.defaultSize
    , buyingConfig = Filters.InvestEverything
    , sellingConfig = Filters.SellNothing
    }


setPortfolio : Portfolio -> StrategyConfiguration -> StrategyConfiguration
setPortfolio portfolio strategy =
    let
        portfolioShares =
            case portfolio of
                Conservative ->
                    PredefinedShares.conservative

                Balanced ->
                    PredefinedShares.balanced

                Progressive ->
                    PredefinedShares.progressive

                UserDefined ->
                    {- switch to UserDefined leaves the current slider configuration untouched -}
                    strategy.portfolioShares
    in
    case strategy of
        { generalSettings } as settings ->
            { settings
                | generalSettings = { generalSettings | portfolio = portfolio }
                , portfolioShares = portfolioShares
            }


setExitConfig : ExitConfig -> StrategyConfiguration -> StrategyConfiguration
setExitConfig exitConfig ({ generalSettings } as config) =
    { config | generalSettings = { generalSettings | exitConfig = exitConfig } }


setTargetPortfolioSize : TargetPortfolioSize -> StrategyConfiguration -> StrategyConfiguration
setTargetPortfolioSize targetPortfolioSize ({ generalSettings } as config) =
    { config | generalSettings = { generalSettings | targetPortfolioSize = targetPortfolioSize } }


setDefaultInvestmentShare : InvestmentShare -> StrategyConfiguration -> StrategyConfiguration
setDefaultInvestmentShare share ({ generalSettings } as config) =
    { config | generalSettings = { generalSettings | defaultInvestmentShare = share } }


updateNotificationSettings : ConfirmationFormMsg -> StrategyConfiguration -> StrategyConfiguration
updateNotificationSettings msg ({ generalSettings } as config) =
    { config
        | generalSettings = { generalSettings | confirmationSettings = Confirmation.update msg generalSettings.confirmationSettings }
    }


setPortfolioShareRange : Rating -> RangeSlider.Msg -> StrategyConfiguration -> StrategyConfiguration
setPortfolioShareRange rtg msg config =
    let
        sharesUpdater : PortfolioShares -> PortfolioShares
        sharesUpdater =
            Dict.Any.update rtg (Maybe.map (RangeSlider.update msg))
    in
    { config | portfolioShares = sharesUpdater config.portfolioShares }


setInvestment : Rating -> RangeSlider.Msg -> StrategyConfiguration -> StrategyConfiguration
setInvestment rtg msg config =
    let
        investmentUpdater : InvestmentsPerRating -> InvestmentsPerRating
        investmentUpdater =
            Dict.Any.update rtg (Maybe.map (RangeSlider.update msg))
    in
    { config | investmentSizeOverrides = investmentUpdater config.investmentSizeOverrides }


setDefaultInvestment : RangeSlider.Msg -> StrategyConfiguration -> StrategyConfiguration
setDefaultInvestment msg config =
    let
        setDefaultInvestmentHelper : GeneralSettings -> GeneralSettings
        setDefaultInvestmentHelper generalSettings =
            { generalSettings | defaultInvestmentSize = RangeSlider.update msg generalSettings.defaultInvestmentSize }

        newGeneralSettings =
            setDefaultInvestmentHelper config.generalSettings
    in
    { config
        | generalSettings = newGeneralSettings
        , investmentSizeOverrides = Investment.defaultInvestmentsPerRating newGeneralSettings.defaultInvestmentSize
    }


setTargetBalance : TargetBalance -> StrategyConfiguration -> StrategyConfiguration
setTargetBalance newBalance ({ generalSettings } as config) =
    { config
        | generalSettings = { generalSettings | defaultTargetBalance = newBalance }
    }


setReservationSetting : ReservationSetting -> StrategyConfiguration -> StrategyConfiguration
setReservationSetting reservationSetting ({ generalSettings } as config) =
    { config
        | generalSettings = { generalSettings | reservationSetting = reservationSetting }
    }


removeBuyFilter : Int -> StrategyConfiguration -> StrategyConfiguration
removeBuyFilter index config =
    { config | buyingConfig = Filters.updateBuyFilters (List.Extra.removeAt index) config.buyingConfig }


removeSellFilter : Int -> StrategyConfiguration -> StrategyConfiguration
removeSellFilter index config =
    { config | sellingConfig = Filters.removeSellFilterAt index config.sellingConfig }


setBuyingConfiguration : Filters.BuyingConfiguration -> StrategyConfiguration -> StrategyConfiguration
setBuyingConfiguration buyingConfiguration strategy =
    { strategy | buyingConfig = buyingConfiguration }


setSellingConfiguration : Filters.SellingConfiguration -> StrategyConfiguration -> StrategyConfiguration
setSellingConfiguration sellingConfiguration strategy =
    { strategy | sellingConfig = sellingConfiguration }


togglePrimaryMarket : Bool -> StrategyConfiguration -> StrategyConfiguration
togglePrimaryMarket enable strategy =
    { strategy | buyingConfig = Filters.togglePrimaryEnablement enable strategy.buyingConfig }


toggleSecondaryMarket : Bool -> StrategyConfiguration -> StrategyConfiguration
toggleSecondaryMarket enable strategy =
    { strategy | buyingConfig = Filters.toggleSecondaryEnablement enable strategy.buyingConfig }


addBuyFilter : MarketplaceFilter -> StrategyConfiguration -> StrategyConfiguration
addBuyFilter newFilter config =
    { config | buyingConfig = Filters.updateBuyFilters (\fs -> fs ++ [ newFilter ]) config.buyingConfig }


addSellFilter : MarketplaceFilter -> StrategyConfiguration -> StrategyConfiguration
addSellFilter newFilter config =
    { config | sellingConfig = Filters.addSellFilter newFilter config.sellingConfig }


renderStrategyConfiguration : BaseUrl -> Posix -> StrategyConfiguration -> String
renderStrategyConfiguration baseUrl generatedOn ({ generalSettings, portfolioShares, investmentSizeOverrides, buyingConfig, sellingConfig } as strategyConfig) =
    Util.joinNonemptyLines
        [ Version.strategyComment generatedOn
        , Version.robozonkyVersionStatement
        , renderGeneralSettings generalSettings
        , PortfolioStructure.renderPortfolioShares generalSettings.portfolio portfolioShares
        , Investment.renderInvestments generalSettings.defaultInvestmentSize investmentSizeOverrides
        , Filters.renderBuyingConfiguration buyingConfig
        , Filters.renderSellingConfiguration sellingConfig
        , shareableUrlComment baseUrl strategyConfig
        ]


renderGeneralSettings : GeneralSettings -> String
renderGeneralSettings generalSettings =
    Util.joinNonemptyLines
        [ "- Obecná nastavení"
        , Portfolio.render generalSettings.portfolio
        , ReservationSetting.render generalSettings.reservationSetting
        , ExitConfig.render generalSettings.exitConfig
        , TargetPortfolioSize.render generalSettings.targetPortfolioSize
        , Investment.renderSize generalSettings.defaultInvestmentSize
        , InvestmentShare.render generalSettings.defaultInvestmentShare
        , TargetBalance.render generalSettings.defaultTargetBalance
        , Confirmation.render generalSettings.confirmationSettings
        ]


validateStrategyConfiguration : StrategyConfiguration -> List String
validateStrategyConfiguration strategyConfig =
    List.concat
        [ validateGeneralSettings strategyConfig.generalSettings
        , PortfolioStructure.validate strategyConfig.portfolioShares
        , Filters.validateSellingConfiguration strategyConfig.sellingConfig
        ]


validateGeneralSettings : GeneralSettings -> List String
validateGeneralSettings generalSettings =
    List.concat
        [ ExitConfig.validate generalSettings.exitConfig
        , Confirmation.validate generalSettings.confirmationSettings
        , TargetPortfolioSize.validate generalSettings.targetPortfolioSize
        , InvestmentShare.validate generalSettings.defaultInvestmentShare
        , TargetBalance.validate generalSettings.defaultTargetBalance
        ]


{-| Strategy equality
-}
strategyEqual : StrategyConfiguration -> StrategyConfiguration -> Bool
strategyEqual s1 s2 =
    Util.and
        [ generalSettingsEqual s1.generalSettings s2.generalSettings
        , PortfolioStructure.portfolioSharesEqual s1.portfolioShares s2.portfolioShares
        , Investment.investmentsPerRatingEqual s1.investmentSizeOverrides s2.investmentSizeOverrides
        , s1.buyingConfig == s2.buyingConfig
        , s1.sellingConfig == s2.sellingConfig
        ]


generalSettingsEqual : GeneralSettings -> GeneralSettings -> Bool
generalSettingsEqual gs1 gs2 =
    Util.and
        [ gs1.portfolio == gs2.portfolio
        , gs1.exitConfig == gs2.exitConfig
        , gs1.targetPortfolioSize == gs2.targetPortfolioSize
        , Investment.investmentSizeEqual gs1.defaultInvestmentSize gs2.defaultInvestmentSize
        , gs1.defaultInvestmentShare == gs2.defaultInvestmentShare
        , gs1.defaultTargetBalance == gs2.defaultTargetBalance
        , Confirmation.equal gs1.confirmationSettings gs2.confirmationSettings
        ]



-- JSON


encodeGeneralSettings : GeneralSettings -> Value
encodeGeneralSettings { portfolio, exitConfig, targetPortfolioSize, defaultInvestmentSize, defaultInvestmentShare, defaultTargetBalance, confirmationSettings, reservationSetting } =
    Encode.object
        [ ( "a", Portfolio.encode portfolio )
        , ( "b", ExitConfig.encode exitConfig )
        , ( "c", TargetPortfolioSize.encode targetPortfolioSize )
        , ( "d", Investment.encodeSize defaultInvestmentSize )
        , ( "e", InvestmentShare.encode defaultInvestmentShare )
        , ( "f", TargetBalance.encode defaultTargetBalance )
        , ( "g", Confirmation.encode confirmationSettings )
        , ( "g1", ReservationSetting.encode reservationSetting )
        ]


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


encodeStrategy : StrategyConfiguration -> Value
encodeStrategy { generalSettings, portfolioShares, investmentSizeOverrides, buyingConfig, sellingConfig } =
    Encode.object
        [ ( "h", encodeGeneralSettings generalSettings )
        , ( "i", PortfolioStructure.encode portfolioShares )
        , ( "j", Investment.encode investmentSizeOverrides )
        , ( "k", Filters.encodeBuyingConfiguration buyingConfig )
        , ( "l", Filters.encodeSellingConfiguration sellingConfig )
        ]


strategyDecoder : Decoder StrategyConfiguration
strategyDecoder =
    Decode.map5 StrategyConfiguration
        (Decode.field "h" generalSettingsDecoder)
        (Decode.field "i" PortfolioStructure.decoder)
        (Decode.field "j" Investment.decoder)
        (Decode.field "k" Filters.decodeBuyingConfiguration)
        (Decode.field "l" Filters.decodeSellingConfiguration)


strategyToUrlHash : StrategyConfiguration -> UrlHash
strategyToUrlHash strategyConfiguration =
    encodeStrategy strategyConfiguration
        |> Encode.encode 0
        |> (\strategyJson -> "2;" ++ strategyJson)
        |> Base64.encode


shareableUrlComment : BaseUrl -> StrategyConfiguration -> String
shareableUrlComment baseUrl strategyConfig =
    let
        urlHash =
            strategyToUrlHash strategyConfig
    in
    String.join "\n"
        [ "# ----------------------------------------------------------------------"
        , "# Pro budoucí úpravy této strategie vložte následující URL do prohlížeče"

        {- This line has to end with a newline, so it's accepted by RoboZonky parser as a comment -}
        , "# " ++ baseUrl ++ "#" ++ urlHash ++ "\n"
        ]
