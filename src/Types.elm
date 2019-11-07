module Types exposing
    ( BaseUrl
    , CreationModalMsg(..)
    , Msg(..)
    , UrlHash
    )

import Bootstrap.Accordion as Accordion
import Bootstrap.Popover as Popover
import Browser
import Data.ExitConfig as ExitConfig
import Data.Filter exposing (BuyingConfiguration, FilteredItem, SellingConfiguration)
import Data.Filter.Complexity exposing (FilterComplexity)
import Data.Filter.Conditions.Rating exposing (Rating)
import Data.Portfolio exposing (Portfolio)
import Data.ReservationSetting exposing (ReservationSetting)
import Data.Tooltip exposing (TipId)
import Percentage
import RangeSlider
import Time exposing (Posix)
import View.Filter.Conditions as Conditions


type Msg
    = PortfolioChanged Portfolio
    | ExitConfigChanged ExitConfig.ExitConfig
    | TargetPortfolioSizeChanged String
    | TargetPortfolioShareChanged String
    | ChangePortfolioSharePercentage Rating Percentage.Msg
    | ChangeInvestment Rating RangeSlider.Msg
    | ChangeDefaultInvestment RangeSlider.Msg
    | SetReservationSetting ReservationSetting
    | RemoveBuyFilter Int
    | RemoveSellFilter Int
    | SellingConfigChanged SellingConfiguration
    | SetSellingConfig SellingConfiguration
    | SetBuyingConfig BuyingConfiguration
    | TogglePrimaryMarket Bool
    | ToggleSecondaryMarket Bool
    | AccordionMsg Accordion.State
    | CreationModalMsg CreationModalMsg
    | TooltipMsg TipId Popover.State
    | SetDateTime Posix
    | DismissAlert
    | LoadUrl Browser.UrlRequest
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


type alias BaseUrl =
    String


type alias UrlHash =
    String
