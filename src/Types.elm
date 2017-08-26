module Types exposing (..)

import Data.Filter exposing (MarketplaceFilter)
import Data.Portfolio exposing (Portfolio)
import Data.Rating exposing (Rating)


type Msg
    = PortfolioChanged Portfolio
    | TargetPortfolioSizeChanged String
    | TargetPortfolioShareChanged String
    | ChangePortfolioShareMin Rating String
    | ChangePortfolioShareMax Rating String
    | ToggleNotificationOnRating Rating Bool
    | ChangeInvestmentMin Rating String
    | ChangeInvestmentMax Rating String
    | ChangeDefaultInvestmentMin String
    | ChangeDefaultInvestmentMax String
    | TargetBalanceChanged String
    | AddBuyFilter MarketplaceFilter
    | RemoveBuyFilter Int
