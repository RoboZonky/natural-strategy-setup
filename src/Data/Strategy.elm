module Data.Strategy exposing (..)

import Data.BuyFilter exposing (BuyFilter(..))
import Data.InvestmentShare as InvestmentShare exposing (InvestmentShare)
import Data.InvestmentSize as InvestmentSize exposing (InvestmentSize(..))
import Data.Portfolio as Portfolio exposing (Portfolio)
import Data.PortfolioShare as PortfolioShare exposing (PortfolioShare(..), Share(..))
import Data.Rating exposing (Rating(..))
import Data.SellFilter exposing (SellFilter(..))
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize)
import Util


type ParsedStrategy
    = SimpleStrategy Portfolio
    | ComplexStrategy ComplexStrategyParameters


type alias ComplexStrategyParameters =
    { generalSettings : GeneralSettings
    , portfolioShares : List PortfolioShare
    , investmentSizes : List InvestmentSize
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
            }
        , portfolioShares = [ PortfolioShare A_Double_Star (Exact 50), PortfolioShare A_Star (Exact 30) ]
        , investmentSizes = []
        , buyFilters = []
        , sellFilters = []
        }


type alias GeneralSettings =
    { portfolio : Portfolio
    , targetPortfolioSize : TargetPortfolioSize
    , defaultInvestmentSize : InvestmentSize
    , defaultInvestmentShare : InvestmentShare
    }


setPortfolio : Portfolio -> ParsedStrategy -> ParsedStrategy
setPortfolio portfolio strategy =
    case strategy of
        SimpleStrategy _ ->
            SimpleStrategy portfolio

        ComplexStrategy ({ generalSettings } as settings) ->
            ComplexStrategy { settings | generalSettings = { generalSettings | portfolio = portfolio } }


setTargetPortfolioSize : TargetPortfolioSize -> ParsedStrategy -> ParsedStrategy
setTargetPortfolioSize targetPortfolioSize strategy =
    case strategy of
        ComplexStrategy ({ generalSettings } as settings) ->
            ComplexStrategy { settings | generalSettings = { generalSettings | targetPortfolioSize = targetPortfolioSize } }

        simple ->
            simple


renderParsedStrategy : ParsedStrategy -> String
renderParsedStrategy strategy =
    case strategy of
        SimpleStrategy portfolioType ->
            Portfolio.renderPortfolio portfolioType

        ComplexStrategy strategyParameters ->
            Util.joinNonemptyLines
                [ renderGeneralSettings strategyParameters.generalSettings
                , PortfolioShare.renderPortfolioShares strategyParameters.portfolioShares
                ]


renderGeneralSettings : GeneralSettings -> String
renderGeneralSettings generalSettings =
    Util.joinNonemptyLines
        [ "- Obecná nastavení"
        , Portfolio.renderPortfolio generalSettings.portfolio
        , TargetPortfolioSize.renderTargetPortfolioSize generalSettings.targetPortfolioSize
        , InvestmentSize.renderInvestmentSizeDefault generalSettings.defaultInvestmentSize
        ]
