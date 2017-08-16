module Types exposing (..)

import Data.Strategy exposing (..)
import Data.Portfolio exposing (Portfolio)


type Msg
    = SimpleStrategySelected
    | ComplexStrategySelected
    | PortfolioChanged Portfolio
    | TargetPortfolioSizeChanged String


type alias Model =
    ParsedStrategy
