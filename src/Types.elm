module Types exposing (..)

import Bootstrap.Accordion as Accordion
import Bootstrap.Modal as Modal
import Data.Filter exposing (Condition, FilteredItem, MarketplaceFilter)
import Data.Filter.Condition.Amount exposing (AmountMsg)
import Data.Portfolio exposing (Portfolio)
import Data.Rating exposing (Rating)


type Msg
    = PortfolioChanged Portfolio
    | TargetPortfolioSizeChanged String
    | TargetPortfolioShareChanged String
    | ChangePortfolioShareMin Rating String
    | ChangePortfolioShareMax Rating String
    | ToggleNotificationOnRating Rating Bool
    | ChangeInvestmentMin Rating String
    | ChangeInvestmentMax Rating String
    | ChangeDefaultInvestmentMin String
    | ChangeDefaultInvestmentMax String
    | TargetBalanceChanged String
    | AddBuyFilter MarketplaceFilter
    | RemoveBuyFilter Int
    | AccordionMsg Accordion.State
    | ModalMsg ModalMsg
    | NoOp


type ModalMsg
    = FilteredItemChange FilteredItem
    | OpenOrClose Modal.State
    | AmountMsg AmountMsg
    | AddCondition Condition
    | RemoveAmountCondition
    | ModalNoOp
