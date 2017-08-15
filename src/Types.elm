module Types exposing (..)

import Data.Strategy exposing (..)
import Data.Strategy.Portfolio as Portfolio


type Msg
    = SimpleStrategySelected
    | ComplexStrategySelected
    | PortfolioChanged Portfolio.DefaultPortfolio
    | TargetPortfolioSizeChanged String


type alias Model =
    ParsedStrategy
