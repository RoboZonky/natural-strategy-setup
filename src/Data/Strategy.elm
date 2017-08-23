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
import Data.TargetBalance as TargetBalance exposing (TargetBalance)
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize)
import Util


type ParsedStrategy
    = SimpleStrategy Portfolio
    | ComplexStrategy ComplexStrategyParameters


type alias ComplexStrategyParameters =
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


defaultSimpleStrategy : ParsedStrategy
defaultSimpleStrategy =
    SimpleStrategy Portfolio.Conservative


defaultComplexStrategy : ParsedStrategy
defaultComplexStrategy =
    ComplexStrategy
        { generalSettings =
            { portfolio = Portfolio.Conservative
            , targetPortfolioSize = TargetPortfolioSize.Unbounded
            , defaultInvestmentSize = ( 200, 200 )
            , defaultInvestmentShare = InvestmentShare.Unbounded
            , defaultTargetBalance = TargetBalance.Unspecified
            , confirmationSettings = Confirmation.confirmationsDisabled
            }
        , portfolioShares = PredefinedShares.conservativeShares
        , investmentSizeOverrides = Investment.defaultInvestmentsPerRating ( 200, 200 )
        , buyFilters = []
        , sellFilters = []
        }


setPortfolio : Portfolio -> ParsedStrategy -> ParsedStrategy
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
        SimpleStrategy _ ->
            SimpleStrategy portfolio

        ComplexStrategy ({ generalSettings } as settings) ->
            ComplexStrategy
                { settings
                    | generalSettings = { generalSettings | portfolio = portfolio }
                    , portfolioShares = portfolioShares
                }


setTargetPortfolioSize : TargetPortfolioSize -> ParsedStrategy -> ParsedStrategy
setTargetPortfolioSize targetPortfolioSize =
    let
        sizeSetter ({ generalSettings } as settings) =
            { settings | generalSettings = { generalSettings | targetPortfolioSize = targetPortfolioSize } }
    in
    modifyComplexStrategy sizeSetter


setNotification : Rating -> Bool -> ParsedStrategy -> ParsedStrategy
setNotification rating isEnabled =
    let
        ratingSetter ({ generalSettings } as settings) =
            { settings
                | generalSettings = { generalSettings | confirmationSettings = AllDict.insert rating isEnabled generalSettings.confirmationSettings }
            }
    in
    modifyComplexStrategy ratingSetter


modifyComplexStrategy : (ComplexStrategyParameters -> ComplexStrategyParameters) -> ParsedStrategy -> ParsedStrategy
modifyComplexStrategy modifierFunction strategy =
    case strategy of
        ComplexStrategy params ->
            ComplexStrategy (modifierFunction params)

        simple ->
            simple


setPortfolioShareMin : Rating -> Int -> ParsedStrategy -> ParsedStrategy
setPortfolioShareMin rtg newMin =
    let
        sharesUpdater =
            AllDict.update rtg (Maybe.map (\( mi, ma ) -> ( newMin, max ma newMin {- automatically bump ma when newMin exceeds it -} )))

        strategyParamsUpdater params =
            { params | portfolioShares = sharesUpdater params.portfolioShares }
    in
    modifyComplexStrategy strategyParamsUpdater


setPortfolioShareMax : Rating -> Int -> ParsedStrategy -> ParsedStrategy
setPortfolioShareMax rtg newMax =
    let
        sharesUpdater =
            AllDict.update rtg (Maybe.map (\( mi, ma ) -> ( mi, newMax )))

        strategyParamsUpdater params =
            { params | portfolioShares = sharesUpdater params.portfolioShares }
    in
    modifyComplexStrategy strategyParamsUpdater


setInvestmentMin : Rating -> Int -> ParsedStrategy -> ParsedStrategy
setInvestmentMin rtg newMin =
    let
        invUpdater =
            AllDict.update rtg (Maybe.map (\( mi, ma ) -> ( newMin, max ma newMin {- automatically bump ma when newMin exceeds it -} )))

        strategyParamsUpdater params =
            { params | investmentSizeOverrides = invUpdater params.investmentSizeOverrides }
    in
    modifyComplexStrategy strategyParamsUpdater


setInvestmentMax : Rating -> Int -> ParsedStrategy -> ParsedStrategy
setInvestmentMax rtg newMax =
    let
        invUpdater =
            AllDict.update rtg (Maybe.map (\( mi, ma ) -> ( mi, newMax )))

        strategyParamsUpdater params =
            { params | investmentSizeOverrides = invUpdater params.investmentSizeOverrides }
    in
    modifyComplexStrategy strategyParamsUpdater


setDefaultInvestmentMin : Int -> ParsedStrategy -> ParsedStrategy
setDefaultInvestmentMin newMin =
    let
        setMin newMin generalSettings =
            { generalSettings | defaultInvestmentSize = ( newMin, max (Tuple.second generalSettings.defaultInvestmentSize) newMin ) }

        strategyParamsUpdater params =
            { params
                | generalSettings = setMin newMin params.generalSettings
                , investmentSizeOverrides = Investment.defaultInvestmentsPerRating ( newMin, max (Tuple.second params.generalSettings.defaultInvestmentSize) newMin )
            }
    in
    modifyComplexStrategy strategyParamsUpdater


setDefaultInvestmentMax : Int -> ParsedStrategy -> ParsedStrategy
setDefaultInvestmentMax newMax =
    let
        setMax newMax generalSettings =
            { generalSettings | defaultInvestmentSize = ( Tuple.first generalSettings.defaultInvestmentSize, newMax ) }

        strategyParamsUpdater params =
            { params
                | generalSettings = setMax newMax params.generalSettings
                , investmentSizeOverrides = Investment.defaultInvestmentsPerRating ( Tuple.first params.generalSettings.defaultInvestmentSize, newMax )
            }
    in
    modifyComplexStrategy strategyParamsUpdater


renderParsedStrategy : ParsedStrategy -> String
renderParsedStrategy strategy =
    case strategy of
        SimpleStrategy portfolioType ->
            Portfolio.renderPortfolio portfolioType

        ComplexStrategy { generalSettings, portfolioShares, investmentSizeOverrides } ->
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
        , Investment.renderDefaultInvestmentSize generalSettings.defaultInvestmentSize
        , TargetBalance.renderTargetBalance generalSettings.defaultTargetBalance
        , Confirmation.renderConfirmation generalSettings.confirmationSettings
        ]
