module Data.Filter exposing (..)

import Data.Filter.Condition.Amount exposing (AmountCondition)
import Data.Filter.Condition.Interest exposing (InterestCondition)
import Data.Filter.Condition.LoanPurpose exposing (LoanPurposeCondition)
import Data.Filter.Condition.LoanTerm exposing (TermCondition)
import Data.Filter.Condition.MainIncome exposing (IncomeCondition)
import Data.Filter.Condition.Region exposing (RegionCondition)
import Data.Filter.Condition.Story exposing (StoryCondition)
import Data.Rating exposing (RatingCondition)


type MarketplaceFilter
    = MarketplaceFilter
        { ignoreWhen : List Condition
        , butNotWhen : List Condition
        }


type Condition
    = Condition_Region RegionCondition
    | Condition_Rating RatingCondition
    | Condition_Income IncomeCondition
    | Condition_Purpose LoanPurposeCondition
    | Condition_Story StoryCondition
    | Condition_Term TermCondition
    | Condition_Amount AmountCondition
    | Condition_Interest InterestCondition
