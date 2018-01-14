module Data.Strategy
    exposing
        ( GeneralSettings
        , StrategyConfiguration
        , addBuyFilter
        , addSellFilter
        , defaultStrategyConfiguration
        , encodeStrategy
        , removeBuyFilter
        , removeSellFilter
        , renderStrategyConfiguration
        , setBuyConf
        , setBuyingConfiguration
        , setDefaultInvestment
        , setDefaultInvestmentShare
        , setInvestment
        , setPortfolio
        , setPortfolioShareRange
        , setSellConf
        , setSellingConfiguration
        , setTargetBalance
        , setTargetPortfolioSize
        , strategyDecoder
        , togglePrimaryMarket
        , toggleSecondaryMarket
        , updateNotificationSettings
        , validateStrategyConfiguration
        )

import AllDict
import Data.Confirmation as Confirmation exposing (ConfirmationSettings)
import Data.Filter as Filters exposing (BuyingConfiguration, MarketplaceFilter, SellingConfiguration)
import Data.Filter.Conditions.Rating as Rating exposing (Rating(..), RatingMsg)
import Data.Investment as Investment exposing (InvestmentsPerRating)
import Data.InvestmentShare as InvestmentShare exposing (InvestmentShare)
import Data.Portfolio as Portfolio exposing (Portfolio(..))
import Data.PortfolioStructure as PortfolioStructure exposing (PortfolioShares)
import Data.PortfolioStructure.PredefinedShares as PredefinedShares
import Data.TargetBalance as TargetBalance exposing (TargetBalance)
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra
import RangeSlider
import Time.DateTime exposing (DateTime)
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
    , targetPortfolioSize : TargetPortfolioSize
    , defaultInvestmentSize : Investment.Size
    , defaultInvestmentShare : InvestmentShare
    , defaultTargetBalance : TargetBalance
    , confirmationSettings : ConfirmationSettings
    }


defaultStrategyConfiguration : StrategyConfiguration
defaultStrategyConfiguration =
    { generalSettings =
        { portfolio = Portfolio.Conservative
        , targetPortfolioSize = TargetPortfolioSize.NotSpecified
        , defaultInvestmentSize = Investment.defaultSize
        , defaultInvestmentShare = InvestmentShare.NotSpecified
        , defaultTargetBalance = TargetBalance.NotSpecified
        , confirmationSettings = Confirmation.confirmationsDisabled
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

                Empty ->
                    PredefinedShares.empty
    in
    case strategy of
        { generalSettings } as settings ->
            { settings
                | generalSettings = { generalSettings | portfolio = portfolio }
                , portfolioShares = portfolioShares
            }


setTargetPortfolioSize : TargetPortfolioSize -> StrategyConfiguration -> StrategyConfiguration
setTargetPortfolioSize targetPortfolioSize ({ generalSettings } as config) =
    { config | generalSettings = { generalSettings | targetPortfolioSize = targetPortfolioSize } }


setDefaultInvestmentShare : InvestmentShare -> StrategyConfiguration -> StrategyConfiguration
setDefaultInvestmentShare share ({ generalSettings } as config) =
    { config | generalSettings = { generalSettings | defaultInvestmentShare = share } }


updateNotificationSettings : RatingMsg -> StrategyConfiguration -> StrategyConfiguration
updateNotificationSettings msg ({ generalSettings } as config) =
    { config
        | generalSettings = { generalSettings | confirmationSettings = Rating.update msg generalSettings.confirmationSettings }
    }


setPortfolioShareRange : Rating -> RangeSlider.Msg -> StrategyConfiguration -> StrategyConfiguration
setPortfolioShareRange rtg msg config =
    let
        sharesUpdater : PortfolioShares -> PortfolioShares
        sharesUpdater =
            AllDict.update rtg (Maybe.map (RangeSlider.update msg))
    in
    { config | portfolioShares = sharesUpdater config.portfolioShares }


setInvestment : Rating -> RangeSlider.Msg -> StrategyConfiguration -> StrategyConfiguration
setInvestment rtg msg config =
    let
        investmentUpdater : InvestmentsPerRating -> InvestmentsPerRating
        investmentUpdater =
            AllDict.update rtg (Maybe.map (RangeSlider.update msg))
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


removeBuyFilter : Int -> StrategyConfiguration -> StrategyConfiguration
removeBuyFilter index config =
    { config | buyingConfig = Filters.updateBuyFilters (List.Extra.removeAt index) config.buyingConfig }


removeSellFilter : Int -> StrategyConfiguration -> StrategyConfiguration
removeSellFilter index config =
    { config | sellingConfig = Filters.updateSellFilters (List.Extra.removeAt index) config.sellingConfig }


setBuyingConfiguration : Filters.BuyingConfiguration -> StrategyConfiguration -> StrategyConfiguration
setBuyingConfiguration buyingConfiguration strategy =
    { strategy | buyingConfig = buyingConfiguration }


setSellingConfiguration : Filters.SellingConfiguration -> StrategyConfiguration -> StrategyConfiguration
setSellingConfiguration sellingConfiguration strategy =
    { strategy | sellingConfig = sellingConfiguration }


setBuyConf : Filters.BuyConf -> StrategyConfiguration -> StrategyConfiguration
setBuyConf buyConf =
    setBuyingConfiguration (Filters.fromBuyConfEnum buyConf)


setSellConf : Filters.SellConf -> StrategyConfiguration -> StrategyConfiguration
setSellConf sellConf =
    setSellingConfiguration (Filters.fromSellConfEnum sellConf)


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
    { config | sellingConfig = Filters.updateSellFilters (\fs -> fs ++ [ newFilter ]) config.sellingConfig }


renderStrategyConfiguration : DateTime -> StrategyConfiguration -> String
renderStrategyConfiguration generatedOn strategy =
    case strategy of
        { generalSettings, portfolioShares, investmentSizeOverrides, buyingConfig, sellingConfig } ->
            Util.joinNonemptyLines
                [ Version.strategyComment generatedOn
                , Version.robozonkyVersionStatement
                , renderGeneralSettings generalSettings
                , PortfolioStructure.renderPortfolioShares generalSettings.portfolio portfolioShares
                , Investment.renderInvestments generalSettings.defaultInvestmentSize investmentSizeOverrides
                , Filters.renderBuyingConfiguration buyingConfig
                , Filters.renderSellingConfiguration sellingConfig
                ]


renderGeneralSettings : GeneralSettings -> String
renderGeneralSettings generalSettings =
    Util.joinNonemptyLines
        [ "- Obecná nastavení"
        , Portfolio.render generalSettings.portfolio
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
        [ TargetPortfolioSize.validate generalSettings.targetPortfolioSize
        , InvestmentShare.validate generalSettings.defaultInvestmentShare
        , TargetBalance.validate generalSettings.defaultTargetBalance
        ]



-- JSON


encodeGeneralSettings : GeneralSettings -> Value
encodeGeneralSettings { portfolio, targetPortfolioSize, defaultInvestmentSize, defaultInvestmentShare, defaultTargetBalance, confirmationSettings } =
    Encode.object
        [ ( "portfolio", Portfolio.encode portfolio )
        , ( "targetPortfolioSize", TargetPortfolioSize.encode targetPortfolioSize )
        , ( "defaultInvestmentSize", Investment.encodeSize defaultInvestmentSize )
        , ( "defaultInvestmentShare", InvestmentShare.encode defaultInvestmentShare )
        , ( "defaultTargetBalance", TargetBalance.encode defaultTargetBalance )
        , ( "confirmationSettings", Confirmation.encode confirmationSettings )
        ]


generalSettingsDecoder : Decoder GeneralSettings
generalSettingsDecoder =
    Decode.map6 GeneralSettings
        (Decode.field "portfolio" Portfolio.decoder)
        (Decode.field "targetPortfolioSize" TargetPortfolioSize.decoder)
        (Decode.field "defaultInvestmentSize" Investment.sizeDecoder)
        (Decode.field "defaultInvestmentShare" InvestmentShare.decoder)
        (Decode.field "defaultTargetBalance" TargetBalance.decoder)
        (Decode.field "confirmationSettings" Confirmation.decoder)


encodeStrategy : StrategyConfiguration -> Value
encodeStrategy { generalSettings, portfolioShares, investmentSizeOverrides, buyingConfig, sellingConfig } =
    Encode.object
        [ ( "generalSettings", encodeGeneralSettings generalSettings )
        , ( "portfolioShares", PortfolioStructure.encode portfolioShares )
        , ( "investmentSizeOverrides", Investment.encode investmentSizeOverrides )
        , ( "buyingConfig", Filters.encodeBuyingConfiguration buyingConfig )
        , ( "sellingConfig", Filters.encodeSellingConfiguration sellingConfig )
        ]


strategyDecoder : Decoder StrategyConfiguration
strategyDecoder =
    Decode.map5 StrategyConfiguration
        (Decode.field "generalSettings" generalSettingsDecoder)
        (Decode.field "portfolioShares" PortfolioStructure.decoder)
        (Decode.field "investmentSizeOverrides" Investment.decoder)
        (Decode.field "buyingConfig" Filters.decodeBuyingConfiguration)
        (Decode.field "sellingConfig" Filters.decodeSellingConfiguration)
