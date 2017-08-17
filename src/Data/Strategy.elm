module Data.Strategy exposing (..)

import Data.BuyFilter exposing (BuyFilter(..))
import Data.Confirmation as Confirmation exposing (Confirmation)
import Data.Investment as Investment exposing (InvestmentPerRating(..), Size(..))
import Data.InvestmentShare as InvestmentShare exposing (InvestmentShare)
import Data.Portfolio as Portfolio exposing (Portfolio)
import Data.PortfolioShare as PortfolioShare exposing (PortfolioShare(..), Share(..))
import Data.Rating exposing (Rating(..), Rating(A_Double_Star))
import Data.SellFilter exposing (SellFilter(..))
import Data.TargetBalance as TargetBalance exposing (TargetBalance)
import Data.TargetPortfolioSize as TargetPortfolioSize exposing (TargetPortfolioSize)
import Util


type ParsedStrategy
    = SimpleStrategy Portfolio
    | ComplexStrategy ComplexStrategyParameters


type alias ComplexStrategyParameters =
    { generalSettings : GeneralSettings
    , portfolioShares : List PortfolioShare
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
        , portfolioShares = [ PortfolioShare A_Double_Star (Exact 50), PortfolioShare A_Star (Exact 30) ]
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
                , Investment.renderInvestments strategyParameters.investmentPerRating
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
