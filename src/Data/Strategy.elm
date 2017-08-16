module Data.Strategy exposing (..)

import Data.BuyFilter exposing (BuyFilter(..))
import Data.InvestmentShare as InvestmentShare exposing (InvestmentShare)
import Data.InvestmentSize exposing (InvestmentSize(..))
import Data.Portfolio as Portfolio exposing (Portfolio)
import Data.PortfolioShare exposing (PortfolioShare(..))
import Data.SellFilter exposing (SellFilter(..))
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize)
import Data.InvestmentSize as InvestmentSize


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
        , portfolioShares = []
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

        ComplexStrategy { generalSettings } ->
            String.join "\n" <|
                List.filter (not << String.isEmpty)
                    [ "- Obecná nastavení"
                    , Portfolio.renderPortfolio generalSettings.portfolio
                    , TargetPortfolioSize.renderTargetPortfolioSize generalSettings.targetPortfolioSize
                    , InvestmentSize.renderInvestmentSizeDefault generalSettings.defaultInvestmentSize
                    ]
