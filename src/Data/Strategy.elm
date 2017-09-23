module Data.Strategy exposing (..)

import AllDict
import Data.Confirmation as Confirmation exposing (ConfirmationSettings)
import Data.Filter as Filters exposing (MarketplaceFilter)
import Data.Filter.Conditions.Rating as Rating exposing (Rating(..), RatingMsg)
import Data.Investment as Investment exposing (InvestmentsPerRating)
import Data.InvestmentShare as InvestmentShare exposing (InvestmentShare)
import Data.Portfolio as Portfolio exposing (Portfolio(..))
import Data.PortfolioStructure as PortfolioStructure exposing (PortfolioShares, Share)
import Data.PortfolioStructure.Predefined as PredefinedShares
import Data.TargetBalance as TargetBalance exposing (TargetBalance)
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize)
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
        { portfolio = Portfolio.Empty
        , targetPortfolioSize = TargetPortfolioSize.NotSpecified
        , defaultInvestmentSize = Investment.defaultSize
        , defaultInvestmentShare = InvestmentShare.NotSpecified
        , defaultTargetBalance = TargetBalance.NotSpecified
        , confirmationSettings = Confirmation.confirmationsDisabled
        }
    , portfolioShares = PredefinedShares.emptyShares
    , investmentSizeOverrides = Investment.defaultInvestmentsPerRating ( 200, 200 )
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


setPortfolioShareRange : Rating -> Share -> StrategyConfiguration -> StrategyConfiguration
setPortfolioShareRange rtg newRange config =
    let
        sharesUpdater =
            AllDict.update rtg (Maybe.map (\_ -> newRange))
    in
    { config | portfolioShares = sharesUpdater config.portfolioShares }


setInvestmentMin : Rating -> Int -> StrategyConfiguration -> StrategyConfiguration
setInvestmentMin rtg newMin config =
    let
        invUpdater =
            AllDict.update rtg (Maybe.map (\( _, ma ) -> ( newMin, max ma newMin {- automatically bump ma when newMin exceeds it -} )))
    in
    { config | investmentSizeOverrides = invUpdater config.investmentSizeOverrides }


setInvestmentMax : Rating -> Int -> StrategyConfiguration -> StrategyConfiguration
setInvestmentMax rtg newMax config =
    let
        invUpdater =
            AllDict.update rtg (Maybe.map (\( mi, _ ) -> ( mi, newMax )))
    in
    { config | investmentSizeOverrides = invUpdater config.investmentSizeOverrides }


setDefaultInvestmentMin : Int -> StrategyConfiguration -> StrategyConfiguration
setDefaultInvestmentMin newMin config =
    let
        setMin newMin generalSettings =
            { generalSettings | defaultInvestmentSize = ( newMin, max (Tuple.second generalSettings.defaultInvestmentSize) newMin ) }
    in
    { config
        | generalSettings = setMin newMin config.generalSettings
        , investmentSizeOverrides = Investment.defaultInvestmentsPerRating ( newMin, max (Tuple.second config.generalSettings.defaultInvestmentSize) newMin )
    }


setDefaultInvestmentMax : Int -> StrategyConfiguration -> StrategyConfiguration
setDefaultInvestmentMax newMax config =
    let
        setMax newMax generalSettings =
            { generalSettings | defaultInvestmentSize = ( Tuple.first generalSettings.defaultInvestmentSize, newMax ) }
    in
    { config
        | generalSettings = setMax newMax config.generalSettings
        , investmentSizeOverrides = Investment.defaultInvestmentsPerRating ( Tuple.first config.generalSettings.defaultInvestmentSize, newMax )
    }


setTargetBalance : TargetBalance -> StrategyConfiguration -> StrategyConfiguration
setTargetBalance newBalance ({ generalSettings } as config) =
    { config
        | generalSettings = { generalSettings | defaultTargetBalance = newBalance }
    }


removeBuyFilter : Int -> StrategyConfiguration -> StrategyConfiguration
removeBuyFilter index config =
    { config | buyFilters = removeItemWithIndex index config.buyFilters }


removeItemWithIndex : Int -> List a -> List a
removeItemWithIndex i xs =
    List.take i xs ++ List.drop (i + 1) xs


addBuyFilter : MarketplaceFilter -> StrategyConfiguration -> StrategyConfiguration
addBuyFilter newFilter config =
    { config | buyFilters = config.buyFilters ++ [ newFilter ] }


renderStrategyConfiguration : StrategyConfiguration -> String
renderStrategyConfiguration strategy =
    case strategy of
        { generalSettings, portfolioShares, investmentSizeOverrides, buyFilters } ->
            Util.joinNonemptyLines
                [ Version.strategyComment
                , renderGeneralSettings generalSettings
                , PortfolioStructure.renderPortfolioShares generalSettings.portfolio portfolioShares
                , Investment.renderInvestments generalSettings.defaultInvestmentSize investmentSizeOverrides
                , Filters.renderFilters buyFilters
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
