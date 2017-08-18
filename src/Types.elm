module Types exposing (..)

import Data.Portfolio exposing (Portfolio)
import Data.Rating exposing (Rating)


type Msg
    = SimpleStrategySelected
    | ComplexStrategySelected
    | PortfolioChanged Portfolio
    | TargetPortfolioSizeChanged String
    | ChangePortfolioShareMin Rating String
    | ChangePortfolioShareMax Rating String
