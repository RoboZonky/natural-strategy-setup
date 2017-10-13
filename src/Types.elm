module Types exposing (..)

import Bootstrap.Accordion as Accordion
import Bootstrap.Modal as Modal
import Bootstrap.Popover as Popover
import Data.Filter exposing (FilteredItem)
import Data.Filter.Conditions.Rating as Rating exposing (Rating)
import Data.Portfolio exposing (Portfolio)
import Data.Tooltip exposing (TipId)
import RangeSlider
import Time exposing (Time)
import View.Filter.Conditions as Conditions


type Msg
    = PortfolioChanged Portfolio
    | TargetPortfolioSizeChanged String
    | TargetPortfolioShareChanged String
    | ChangePortfolioSharePercentage Rating RangeSlider.Msg
    | ConfirmationFormMsg Rating.RatingMsg
    | ChangeInvestment Rating RangeSlider.Msg
    | ChangeDefaultInvestment RangeSlider.Msg
    | TargetBalanceChanged String
    | RemoveBuyFilter Int
    | RemoveSellFilter Int
    | AccordionMsg Accordion.State
    | ModalMsg ModalMsg
    | TooltipMsg TipId Popover.State
    | SetDateTime Time
    | ShareStrategy
    | NoOp


type ModalMsg
    = TogglePositiveNegativeSubform
    | ModalStateMsg FilteredItem Modal.State
    | PositiveConditionsChange Conditions.Msg
    | NegativeConditionsChange Conditions.Msg
    | ModalTooltipMsg TipId Popover.State
    | SaveFilter
    | ModalNoOp
