module Types exposing (CreationModalMsg(..), DeletionModalMsg(..), Msg(..))

import Bootstrap.Accordion as Accordion
import Bootstrap.Modal as Modal
import Bootstrap.Popover as Popover
import Data.Filter as Filter exposing (FilteredItem)
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
    | SetBuyingConfiguration Filter.BuyConf
    | TogglePrimaryMarket Bool
    | ToggleSecondaryMarket Bool
    | SetSellingConfiguration Filter.SellConf
    | AccordionMsg Accordion.State
    | CreationModalMsg CreationModalMsg
    | DeletionModalMsg DeletionModalMsg
    | TooltipMsg TipId Popover.State
    | SetDateTime Time
    | ShareStrategy
    | NoOp


type CreationModalMsg
    = TogglePositiveNegativeSubform
    | ModalStateMsg FilteredItem Modal.State
    | PositiveConditionsChange Conditions.Msg
    | NegativeConditionsChange Conditions.Msg
    | ModalTooltipMsg TipId Popover.State
    | SaveFilter
    | ModalNoOp


type DeletionModalMsg
    = ConfirmDeletion
    | DeletionModalStateMsg Modal.State
