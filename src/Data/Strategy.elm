module Data.Strategy
    exposing
        ( GeneralSettings
        , StrategyConfiguration
        , addBuyFilter
        , addSellFilter
        , defaultStrategyConfiguration
        , removeBuyFilter
        , removeSellFilter
        , renderStrategyConfiguration
        , setDefaultInvestment
        , setDefaultInvestmentShare
        , setInvestment
        , setPortfolio
        , setPortfolioShareRange
        , setTargetBalance
        , setTargetPortfolioSize
        , updateNotificationSettings
        , validateStrategyConfiguration
        )

import AllDict
import Data.Confirmation as Confirmation exposing (ConfirmationSettings)
import Data.Filter as Filters exposing (MarketplaceFilter)
import Data.Filter.Conditions.Rating as Rating exposing (Rating(..), RatingMsg)
import Data.Investment as Investment exposing (InvestmentsPerRating)
import Data.InvestmentShare as InvestmentShare exposing (InvestmentShare)
import Data.Portfolio as Portfolio exposing (Portfolio(..))
import Data.PortfolioStructure as PortfolioStructure exposing (PortfolioShares)
import Data.PortfolioStructure.Predefined as PredefinedShares
import Data.TargetBalance as TargetBalance exposing (TargetBalance)
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize)
import RangeSlider
import Util
import Version


type alias StrategyConfiguration =
    { generalSettings : GeneralSettings
    , portfolioShares : PortfolioShares
    , investmentSizeOverrides : InvestmentsPerRating
    , buyFilters : List MarketplaceFilter
    , sellFilters : List MarketplaceFilter
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
    , portfolioShares = PredefinedShares.conservativeShares
    , investmentSizeOverrides = Investment.defaultInvestmentsPerRating Investment.defaultSize
    , buyFilters = []
    , sellFilters = []
    }


setPortfolio : Portfolio -> StrategyConfiguration -> StrategyConfiguration
setPortfolio portfolio strategy =
    let
        portfolioShares =
            case portfolio of
                Conservative ->
                    PredefinedShares.conservativeShares

                Balanced ->
                    PredefinedShares.balancedShares

                Progressive ->
                    PredefinedShares.progressiveShares

                Empty ->
                    PredefinedShares.emptyShares
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
        setDefaultInvestment : RangeSlider.Msg -> GeneralSettings -> GeneralSettings
        setDefaultInvestment msg generalSettings =
            { generalSettings | defaultInvestmentSize = RangeSlider.update msg generalSettings.defaultInvestmentSize }

        newGeneralSettings =
            setDefaultInvestment msg config.generalSettings
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
    { config | buyFilters = removeItemWithIndex index config.buyFilters }


removeSellFilter : Int -> StrategyConfiguration -> StrategyConfiguration
removeSellFilter index config =
    { config | sellFilters = removeItemWithIndex index config.sellFilters }


removeItemWithIndex : Int -> List a -> List a
removeItemWithIndex i xs =
    List.take i xs ++ List.drop (i + 1) xs


addBuyFilter : MarketplaceFilter -> StrategyConfiguration -> StrategyConfiguration
addBuyFilter newFilter config =
    { config | buyFilters = config.buyFilters ++ [ newFilter ] }


addSellFilter : MarketplaceFilter -> StrategyConfiguration -> StrategyConfiguration
addSellFilter newFilter config =
    { config | sellFilters = config.sellFilters ++ [ newFilter ] }


renderStrategyConfiguration : StrategyConfiguration -> String
renderStrategyConfiguration strategy =
    case strategy of
        { generalSettings, portfolioShares, investmentSizeOverrides, buyFilters, sellFilters } ->
            Util.joinNonemptyLines
                [ Version.strategyComment
                , renderGeneralSettings generalSettings
                , PortfolioStructure.renderPortfolioShares generalSettings.portfolio portfolioShares
                , Investment.renderInvestments generalSettings.defaultInvestmentSize investmentSizeOverrides
                , Filters.renderBuyFilters buyFilters
                , Filters.renderSellFilters sellFilters
                ]


renderGeneralSettings : GeneralSettings -> String
renderGeneralSettings generalSettings =
    Util.joinNonemptyLines
        [ "- Obecná nastavení"
        , Portfolio.renderPortfolio generalSettings.portfolio
        , TargetPortfolioSize.renderTargetPortfolioSize generalSettings.targetPortfolioSize
        , Investment.renderDefaultInvestmentSize generalSettings.defaultInvestmentSize
        , InvestmentShare.renderInvestmentShare generalSettings.defaultInvestmentShare
        , TargetBalance.renderTargetBalance generalSettings.defaultTargetBalance
        , Confirmation.renderConfirmation generalSettings.confirmationSettings
        ]


validateStrategyConfiguration : StrategyConfiguration -> List String
validateStrategyConfiguration strategyConfig =
    List.concat
        [ validateGeneralSettings strategyConfig.generalSettings
        , PortfolioStructure.validate strategyConfig.portfolioShares
        ]


validateGeneralSettings : GeneralSettings -> List String
validateGeneralSettings generalSettings =
    List.concat
        [ TargetPortfolioSize.validate generalSettings.targetPortfolioSize
        , InvestmentShare.validate generalSettings.defaultInvestmentShare
        , TargetBalance.validate generalSettings.defaultTargetBalance
        ]
