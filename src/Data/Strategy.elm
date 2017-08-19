module Data.Strategy exposing (..)

import AllDict as Dict
import Data.BuyFilter exposing (BuyFilter(..))
import Data.Confirmation as Confirmation exposing (Confirmation)
import Data.Investment as Investment exposing (InvestmentPerRating(..), Size(..))
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
    , investmentPerRating : List InvestmentPerRating
    , buyFilters : List BuyFilter
    , sellFilters : List SellFilter
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
            , defaultInvestmentSize = Amount 200
            , defaultInvestmentShare = InvestmentShare.Unbounded
            , defaultTargetBalance = TargetBalance.Unspecified
            , confirmationSettings = Confirmation.Disabled
            }
        , portfolioShares = PredefinedShares.conservativeShares
        , investmentPerRating = [ InvestmentPerRating A_Double_Star (Amount 150), InvestmentPerRating B (FromTo 10 20), InvestmentPerRating D (UpTo 80) ]
        , buyFilters = []
        , sellFilters = []
        }


type alias GeneralSettings =
    { portfolio : Portfolio
    , targetPortfolioSize : TargetPortfolioSize
    , defaultInvestmentSize : Investment.Size
    , defaultInvestmentShare : InvestmentShare
    , defaultTargetBalance : TargetBalance
    , confirmationSettings : Confirmation
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
            Dict.update rtg (Maybe.map (\( mi, ma ) -> ( newMin, max ma newMin {- automatically bump ma when newMin exceeds it -} )))

        strategyParamsUpdater params =
            { params | portfolioShares = sharesUpdater params.portfolioShares }
    in
    modifyComplexStrategy strategyParamsUpdater


setPortfolioShareMax : Rating -> Int -> ParsedStrategy -> ParsedStrategy
setPortfolioShareMax rtg newMax =
    let
        sharesUpdater =
            Dict.update rtg (Maybe.map (\( mi, ma ) -> ( mi, newMax )))

        strategyParamsUpdater params =
            { params | portfolioShares = sharesUpdater params.portfolioShares }
    in
    modifyComplexStrategy strategyParamsUpdater


renderParsedStrategy : ParsedStrategy -> String
renderParsedStrategy strategy =
    case strategy of
        SimpleStrategy portfolioType ->
            Portfolio.renderPortfolio portfolioType

        ComplexStrategy { generalSettings, portfolioShares, investmentPerRating } ->
            Util.joinNonemptyLines
                [ renderGeneralSettings generalSettings
                , PortfolioShare.renderPortfolioShares generalSettings.portfolio portfolioShares
                , Investment.renderInvestments investmentPerRating
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
