module Types
    exposing
        ( AlertData(..)
        , BaseUrl
        , CreationModalMsg(..)
        , DeletionModalMsg(..)
        , Msg(..)
        )

import Bootstrap.Accordion as Accordion
import Bootstrap.Modal as Modal
import Bootstrap.Popover as Popover
import Data.ExitConfig as ExitConfig
import Data.Filter exposing (FilteredItem)
import Data.Filter.Conditions.Rating as Rating exposing (Rating)
import Data.Portfolio exposing (Portfolio)
import Data.Tooltip exposing (TipId)
import RangeSlider
import Time exposing (Time)
import View.Filter.Conditions as Conditions


type Msg
    = PortfolioChanged Portfolio
    | ExitConfigChanged ExitConfig.ExitConfig
    | TargetPortfolioSizeChanged String
    | TargetPortfolioShareChanged String
    | ChangePortfolioSharePercentage Rating RangeSlider.Msg
    | ConfirmationFormMsg Rating.RatingMsg
    | ChangeInvestment Rating RangeSlider.Msg
    | ChangeDefaultInvestment RangeSlider.Msg
    | TargetBalanceChanged String
    | RemoveBuyFilter Int
    | RemoveSellFilter Int
    | TogglePrimaryMarket Bool
    | ToggleSecondaryMarket Bool
    | AccordionMsg Accordion.State
    | CreationModalMsg CreationModalMsg
    | DeletionModalMsg DeletionModalMsg
    | TooltipMsg TipId Popover.State
    | SetDateTime Time
    | DismisAlert
    | NoOp


type CreationModalMsg
    = TogglePositiveNegativeSubform
    | ModalStateMsg FilteredItem Modal.Visibility
    | PositiveConditionsChange Conditions.Msg
    | NegativeConditionsChange Conditions.Msg
    | ModalTooltipMsg TipId Popover.State
    | SaveFilter
    | ModalNoOp


type DeletionModalMsg
    = ConfirmDeletion
    | DeletionModalStateMsg Modal.Visibility


type alias BaseUrl =
    String


type AlertData
    = SuccessAlert String
    | ErrorAlert String
    | NoAlert
