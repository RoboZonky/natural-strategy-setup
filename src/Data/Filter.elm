module Data.Filter
    exposing
        ( Condition(..)
        , FilteredItem(..)
        , MarketplaceFilter(..)
        , emptyFilter
        , filtereedItemFromString
        , renderFilteredItem
        , renderFilters
        , renderMarketplaceFilter
        , setFilteredItem
        )

import Data.Filter.Condition.Amount exposing (AmountCondition, renderAmountCondition)
import Data.Filter.Condition.Interest exposing (InterestCondition, renderInterestCondition)
import Data.Filter.Condition.LoanPurpose exposing (LoanPurposeCondition, renderLoanPurposeCondition)
import Data.Filter.Condition.LoanTerm exposing (TermCondition, renderTermCondition)
import Data.Filter.Condition.MainIncome exposing (IncomeCondition, renderIncomeCondition)
import Data.Filter.Condition.Region exposing (RegionCondition, renderRegionCondition)
import Data.Filter.Condition.Story exposing (StoryCondition, renderStoryCondition)
import Data.Rating exposing (RatingCondition, renderRatingCondition)
import Util


renderFilters : List MarketplaceFilter -> String
renderFilters filters =
    if List.isEmpty filters then
        ""
    else
        Util.joinNonemptyLines <| "\n- Filtrování tržiště" :: List.map renderMarketplaceFilter filters


type MarketplaceFilter
    = MarketplaceFilter
        { whatToFilter : FilteredItem
        , ignoreWhen : List Condition
        , butNotWhen : List Condition
        }


emptyFilter : MarketplaceFilter
emptyFilter =
    MarketplaceFilter
        { whatToFilter = Loan
        , ignoreWhen = []
        , butNotWhen = []
        }


setFilteredItem : FilteredItem -> MarketplaceFilter -> MarketplaceFilter
setFilteredItem newItem (MarketplaceFilter mf) =
    MarketplaceFilter { mf | whatToFilter = newItem }


renderMarketplaceFilter : MarketplaceFilter -> String
renderMarketplaceFilter (MarketplaceFilter { whatToFilter, ignoreWhen, butNotWhen }) =
    let
        negativePart =
            if List.isEmpty butNotWhen then
                ""
            else
                "\n(Ale ne když: " ++ renderConditionList butNotWhen ++ ")"

        positivePart =
            renderConditionList ignoreWhen
    in
    "Ignorovat " ++ renderFilteredItem whatToFilter ++ ", kde: " ++ positivePart ++ negativePart


renderConditionList : List Condition -> String
renderConditionList =
    List.map renderCondition >> String.join "; " >> addDotIfNotEmpty


addDotIfNotEmpty : String -> String
addDotIfNotEmpty s =
    s
        ++ (if String.isEmpty s then
                ""
            else
                "."
           )


type Condition
    = Condition_Region RegionCondition
    | Condition_Rating RatingCondition
    | Condition_Income IncomeCondition
    | Condition_Purpose LoanPurposeCondition
    | Condition_Story StoryCondition
    | Condition_Term TermCondition
    | Condition_Amount AmountCondition
    | Condition_Interest InterestCondition


renderCondition : Condition -> String
renderCondition condition =
    case condition of
        Condition_Region regionCond ->
            renderRegionCondition regionCond

        Condition_Rating ratingCond ->
            renderRatingCondition ratingCond

        Condition_Income incomeCond ->
            renderIncomeCondition incomeCond

        Condition_Purpose loanPurposeCond ->
            renderLoanPurposeCondition loanPurposeCond

        Condition_Story storyCond ->
            renderStoryCondition storyCond

        Condition_Term termCond ->
            renderTermCondition termCond

        Condition_Amount amountCond ->
            renderAmountCondition amountCond

        Condition_Interest interestCond ->
            renderInterestCondition interestCond


type FilteredItem
    = Loan
    | Participation
    | Loan_And_Participation


renderFilteredItem : FilteredItem -> String
renderFilteredItem item =
    case item of
        Loan_And_Participation ->
            "vše"

        Participation ->
            "participaci"

        Loan ->
            "úvěr"


filtereedItemFromString : String -> FilteredItem
filtereedItemFromString s =
    case s of
        "Loan_And_Participation" ->
            Loan_And_Participation

        "Participation" ->
            Participation

        _ ->
            Loan
