module Data.Strategy exposing (..)

import AllDict
import Data.BuyFilter exposing (BuyFilter(..))
import Data.Confirmation as Confirmation exposing (ConfirmationSettings)
import Data.Investment as Investment exposing (InvestmentsPerRating, Size)
import Data.InvestmentShare as InvestmentShare exposing (InvestmentShare)
import Data.Portfolio as Portfolio exposing (Portfolio(..))
import Data.PortfolioShare as PortfolioShare exposing (PortfolioShare, PortfolioShares, Share)
import Data.PortfolioShare.Predefined as PredefinedShares
import Data.Rating exposing (Rating(..))
import Data.SellFilter exposing (SellFilter(..))
import Data.TargetBalance as TargetBalance exposing (TargetBalance, defaultTargetBalance)
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize)
import Util


type alias StrategyConfiguration =
    { generalSettings : GeneralSettings
    , portfolioShares : PortfolioShares
    , investmentSizeOverrides : InvestmentsPerRating
    , buyFilters : List BuyFilter
    , sellFilters : List SellFilter
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
        , targetPortfolioSize = TargetPortfolioSize.Unbounded
        , defaultInvestmentSize = Investment.defaultSize
        , defaultInvestmentShare = InvestmentShare.Unrestricted
        , defaultTargetBalance = TargetBalance.Unspecified
        , confirmationSettings = Confirmation.confirmationsDisabled
        }
    , portfolioShares = PredefinedShares.conservativeShares
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


setNotification : Rating -> Bool -> StrategyConfiguration -> StrategyConfiguration
setNotification rating isEnabled ({ generalSettings } as config) =
    { config
        | generalSettings = { generalSettings | confirmationSettings = AllDict.insert rating isEnabled generalSettings.confirmationSettings }
    }


setPortfolioShareMin : Rating -> Int -> StrategyConfiguration -> StrategyConfiguration
setPortfolioShareMin rtg newMin config =
    let
        sharesUpdater =
            AllDict.update rtg (Maybe.map (\( mi, ma ) -> ( newMin, max ma newMin {- automatically bump ma when newMin exceeds it -} )))
    in
    { config | portfolioShares = sharesUpdater config.portfolioShares }


setPortfolioShareMax : Rating -> Int -> StrategyConfiguration -> StrategyConfiguration
setPortfolioShareMax rtg newMax config =
    let
        sharesUpdater =
            AllDict.update rtg (Maybe.map (\( mi, ma ) -> ( mi, newMax )))
    in
    { config | portfolioShares = sharesUpdater config.portfolioShares }


setInvestmentMin : Rating -> Int -> StrategyConfiguration -> StrategyConfiguration
setInvestmentMin rtg newMin config =
    let
        invUpdater =
            AllDict.update rtg (Maybe.map (\( mi, ma ) -> ( newMin, max ma newMin {- automatically bump ma when newMin exceeds it -} )))
    in
    { config | investmentSizeOverrides = invUpdater config.investmentSizeOverrides }


setInvestmentMax : Rating -> Int -> StrategyConfiguration -> StrategyConfiguration
setInvestmentMax rtg newMax config =
    let
        invUpdater =
            AllDict.update rtg (Maybe.map (\( mi, ma ) -> ( mi, newMax )))
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


renderStrategyConfiguration : StrategyConfiguration -> String
renderStrategyConfiguration strategy =
    case strategy of
        { generalSettings, portfolioShares, investmentSizeOverrides } ->
            Util.joinNonemptyLines
                [ renderGeneralSettings generalSettings
                , PortfolioShare.renderPortfolioShares generalSettings.portfolio portfolioShares
                , Investment.renderInvestments generalSettings.defaultInvestmentSize investmentSizeOverrides
                ]


renderGeneralSettings : GeneralSettings -> String
renderGeneralSettings generalSettings =
    Util.joinNonemptyLines
        [ "- Obecná nastavení"
        , Portfolio.renderPortfolio generalSettings.portfolio
        , TargetPortfolioSize.renderTargetPortfolioSize generalSettings.targetPortfolioSize
        , InvestmentShare.renderInvestmentShare generalSettings.defaultInvestmentShare
        , Investment.renderDefaultInvestmentSize generalSettings.defaultInvestmentSize
        , TargetBalance.renderTargetBalance generalSettings.defaultTargetBalance
        , Confirmation.renderConfirmation generalSettings.confirmationSettings
        ]
