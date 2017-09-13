module Types exposing (..)

import Bootstrap.Accordion as Accordion
import Bootstrap.Modal as Modal
import Bootstrap.Popover as Popover
import Data.Filter exposing (Condition, FilteredItem, MarketplaceFilter)
import Data.Filter.Condition.Amount exposing (AmountMsg)
import Data.Filter.Condition.Interest exposing (InterestMsg)
import Data.Filter.Condition.LoanPurpose exposing (PurposeMsg)
import Data.Filter.Condition.LoanTerm exposing (LoanTermMsg)
import Data.Filter.Condition.MainIncome exposing (MainIncomeMsg)
import Data.Filter.Condition.Rating as Rating exposing (Rating, RatingMsg)
import Data.Filter.Condition.Region exposing (RegionMsg)
import Data.Filter.Condition.Story exposing (StoryMsg)
import Data.Portfolio exposing (Portfolio)
import Data.Tooltip exposing (TipId)


type Msg
    = PortfolioChanged Portfolio
    | TargetPortfolioSizeChanged String
    | TargetPortfolioShareChanged String
    | ChangePortfolioShareMin Rating String
    | ChangePortfolioShareMax Rating String
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
    | ModalStateMsg Modal.State
    | InterestMsg InterestMsg
    | AmountMsg AmountMsg
    | StoryMsg StoryMsg
    | PurposeMsg PurposeMsg
    | LoanTermMsg LoanTermMsg
    | MainIncomeMsg MainIncomeMsg
    | RatingMsg RatingMsg
    | RegionMsg RegionMsg
    | AddCondition Condition
    | RemoveInterestCondition
    | RemoveAmountCondition
    | RemoveStoryCondition
    | RemovePurposeCondition
    | RemoveTermCondition
    | RemoveMainIncomeCondition
    | RemoveRatingCondition
    | RemoveRegionCondition
    | ModalTooltipMsg TipId Popover.State
    | SaveFilter
    | ModalNoOp
