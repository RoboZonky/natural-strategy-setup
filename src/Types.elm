module Types exposing
    ( BaseUrl
    , CreationModalMsg(..)
    , DeletionModalMsg(..)
    , Msg(..)
    , UrlHash
    )

import Bootstrap.Accordion as Accordion
import Bootstrap.Popover as Popover
import Data.Confirmation as Confirmation
import Data.ExitConfig as ExitConfig
import Data.Filter exposing (FilteredItem)
import Data.Filter.Complexity exposing (FilterComplexity)
import Data.Filter.Conditions.Rating exposing (Rating)
import Data.Portfolio exposing (Portfolio)
import Data.ReservationSetting exposing (ReservationSetting)
import Data.Tooltip exposing (TipId)
import RangeSlider
import Time exposing (Posix)
import View.Filter.Conditions as Conditions


type Msg
    = PortfolioChanged Portfolio
    | ExitConfigChanged ExitConfig.ExitConfig
    | TargetPortfolioSizeChanged String
    | TargetPortfolioShareChanged String
    | ChangePortfolioSharePercentage Rating RangeSlider.Msg
    | ConfirmationFormMsg Confirmation.ConfirmationFormMsg
    | ChangeInvestment Rating RangeSlider.Msg
    | ChangeDefaultInvestment RangeSlider.Msg
    | SetReservationSetting ReservationSetting
    | TargetBalanceChanged String
    | RemoveBuyFilter Int
    | RemoveSellFilter Int
    | TogglePrimaryMarket Bool
    | ToggleSecondaryMarket Bool
    | AccordionMsg Accordion.State
    | CreationModalMsg CreationModalMsg
    | DeletionModalMsg DeletionModalMsg
    | TooltipMsg TipId Popover.State
    | SetDateTime Posix
    | DismisAlert
    | NoOp


type CreationModalMsg
    = TogglePositiveNegativeSubform
    | OpenCreationModal FilterComplexity (List FilteredItem)
    | PositiveConditionsChange Conditions.Msg
    | NegativeConditionsChange Conditions.Msg
    | ModalTooltipMsg TipId Popover.State
    | SetFilteredItem FilteredItem
    | ConfirmConditionsRemoval
    | CancelConditionsRemoval
    | SaveFilter
    | CloseModal
    | ModalNoOp


type DeletionModalMsg
    = ConfirmDeletion
    | CancelDeletion


type alias BaseUrl =
    String


type alias UrlHash =
    String
