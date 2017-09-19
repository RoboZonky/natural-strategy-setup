module Types exposing (..)

import Bootstrap.Accordion as Accordion
import Bootstrap.Modal as Modal
import Bootstrap.Popover as Popover
import Bootstrap.Tab as Tab
import Data.Filter exposing (FilteredItem, MarketplaceFilter)
import Data.Filter.Conditions.Rating as Rating exposing (Rating, RatingMsg)
import Data.Portfolio exposing (Portfolio)
import Data.Tooltip exposing (TipId)
import RangeSlider
import View.Filter.Conditions as Conditions


type Msg
    = PortfolioChanged Portfolio
    | TargetPortfolioSizeChanged String
    | TargetPortfolioShareChanged String
    | ChangePortfolioSharePercentage Rating RangeSlider.Msg
    | ConfirmationFormMsg Rating.RatingMsg
    | ChangeInvestmentMin Rating String
    | ChangeInvestmentMax Rating String
    | ChangeDefaultInvestmentMin String
    | ChangeDefaultInvestmentMax String
    | TargetBalanceChanged String
    | AddBuyFilter MarketplaceFilter
    | RemoveBuyFilter Int
    | AccordionMsg Accordion.State
    | ModalMsg ModalMsg
    | TooltipMsg TipId Popover.State
    | NoOp


type ModalMsg
    = FilteredItemChange FilteredItem
    | ModalStateMsg Bool Modal.State
    | PositiveConditionsChange Conditions.Msg
    | NegativeConditionsChange Conditions.Msg
    | ModalTooltipMsg TipId Popover.State
    | TabMsg Tab.State
    | SaveFilter
    | ModalNoOp
