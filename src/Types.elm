module Types exposing (..)

import Bootstrap.Accordion as Accordion
import Bootstrap.Modal as Modal
import Bootstrap.Popover as Popover
import Data.Filter exposing (FilteredItem, MarketplaceFilter)
import Data.Filter.Conditions exposing (Condition)
import Data.Filter.Conditions.Amount exposing (AmountMsg)
import Data.Filter.Conditions.Interest exposing (InterestMsg)
import Data.Filter.Conditions.LoanPurpose exposing (LoanPurposeMsg)
import Data.Filter.Conditions.LoanTerm exposing (LoanTermMsg)
import Data.Filter.Conditions.MainIncome exposing (MainIncomeMsg)
import Data.Filter.Conditions.Rating as Rating exposing (Rating, RatingMsg)
import Data.Filter.Conditions.Region exposing (RegionMsg)
import Data.Filter.Conditions.Story exposing (StoryMsg)
import Data.Portfolio exposing (Portfolio)
import Data.Tooltip exposing (TipId)
import RangeSlider


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
    | ModalStateMsg Modal.State
    | InterestMsg InterestMsg
    | AmountMsg AmountMsg
    | StoryMsg StoryMsg
    | LoanPurposeMsg LoanPurposeMsg
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
