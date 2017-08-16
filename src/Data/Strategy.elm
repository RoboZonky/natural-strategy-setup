module Data.Strategy exposing (..)

import Data.PortfolioShare exposing (PortfolioShare(..))
import Data.InvestmentSize exposing (InvestmentSize(..))
import Data.BuyFilter exposing (BuyFilter(..))
import Data.SellFilter exposing (SellFilter(..))
import Data.Portfolio as Portfolio exposing (DefaultPortfolio)


type ParsedStrategy
    = SimpleStrategy DefaultPortfolio
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
            , targetPortfolioSize = Unbounded
            , investmentSize = Amount 200
            }
        , portfolioShares = []
        , investmentSizes = []
        , buyFilters = []
        , sellFilters = []
        }


type alias GeneralSettings =
    { portfolio : DefaultPortfolio
    , targetPortfolioSize : TargetPortfolioSize
    , investmentSize : InvestmentSize
    }


setPortfolio : DefaultPortfolio -> ParsedStrategy -> ParsedStrategy
setPortfolio portfolio strategy =
    case strategy of
        SimpleStrategy _ ->
            SimpleStrategy portfolio

        ComplexStrategy ({ generalSettings } as settings) ->
            ComplexStrategy { settings | generalSettings = { generalSettings | portfolio = portfolio } }


renderDefaultPortfolio : DefaultPortfolio -> String
renderDefaultPortfolio portfolio =
    "Robot má udržovat " ++ Portfolio.toString portfolio ++ " portfolio."


type TargetPortfolioSize
    = Unbounded
    | Bounded Int


setTargetPortfolioSize : TargetPortfolioSize -> ParsedStrategy -> ParsedStrategy
setTargetPortfolioSize targetPortfolioSize strategy =
    case strategy of
        ComplexStrategy ({ generalSettings } as settings) ->
            ComplexStrategy { settings | generalSettings = { generalSettings | targetPortfolioSize = targetPortfolioSize } }

        simple ->
            simple


renderTargetPortfolioSize : TargetPortfolioSize -> List String
renderTargetPortfolioSize targetPortfolioSize =
    case targetPortfolioSize of
        Unbounded ->
            []

        Bounded maxBound ->
            [ "Cílová zůstatková částka je " ++ toString maxBound ++ " Kč." ]


renderParsedStrategy : ParsedStrategy -> String
renderParsedStrategy strategy =
    case strategy of
        SimpleStrategy portfolioType ->
            renderDefaultPortfolio portfolioType

        ComplexStrategy { generalSettings } ->
            String.join "\n" <|
                [ "- Obecná nastavení"
                , renderDefaultPortfolio generalSettings.portfolio
                ]
                    ++ renderTargetPortfolioSize generalSettings.targetPortfolioSize
