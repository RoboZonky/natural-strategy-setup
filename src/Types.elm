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
import Data.Investment as Investment
import Data.Portfolio exposing (Portfolio)
import Data.ReservationSetting exposing (ReservationSetting)
import Data.Tooltip exposing (TipId)
import Percentage
import Time exposing (Posix)
import View.Filter.Conditions as Conditions


type Msg
    = PortfolioChanged Portfolio
    | ExitConfigChanged ExitConfig.ExitConfig
    | TargetPortfolioSizeChanged String
    | PortfolioPercentageChanged Rating Percentage.Msg
    | DefaultPrimaryInvestmentChanged Investment.Msg
    | DefaultSecondaryPurchaseChanged Investment.Msg
    | PrimaryInvestmentChanged Rating Investment.Msg
    | SecondaryPurchaseChanged Rating Investment.Msg
    | ReservationSettingChanged ReservationSetting
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
