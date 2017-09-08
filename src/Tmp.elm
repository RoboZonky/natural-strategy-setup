module Tmp exposing (..)

import Data.Filter exposing (..)
import Data.Filter.Condition.Interest as Interest exposing (InterestCondition(..))
import Data.Filter.Condition.LoanPurpose exposing (..)
import Data.Filter.Condition.LoanTerm exposing (..)
import Data.Filter.Condition.MainIncome exposing (MainIncome(..), MainIncomeCondition(..))
import Data.Filter.Condition.Region exposing (..)
import Data.Filter.Condition.Story exposing (..)
import Data.Rating exposing (..)


sampleFilters : List MarketplaceFilter
sampleFilters =
    [ addPositiveCondition (Condition_Region (RegionList [ USTECKY ])) emptyFilter
    , addPositiveCondition (Condition_Purpose (LoanPurposeList [ CESTOVANI, JINE ])) emptyFilter
    , addPositiveCondition (Condition_Income (MainIncomeList [ STUDENT, UNEMPLOYED ])) emptyFilter
    , addPositiveCondition (Condition_Rating (RatingList [ A ])) emptyFilter
    , addPositiveCondition (Condition_Rating (RatingList [ C, D ])) emptyFilter
    , addPositiveCondition (Condition_Rating (RatingList [ A, B, C ])) emptyFilter
    , addPositiveCondition (Condition_Rating (WorseThan A)) emptyFilter
    , addPositiveCondition (Condition_Rating (BetterThan A_Plus)) emptyFilter
    , addPositiveCondition (Condition_Story (StoryCondition SHORT)) emptyFilter
    , addPositiveCondition (Condition_Term (TermCondition (LessThan 36))) emptyFilter
    , addPositiveCondition (Condition_Term (TermCondition (Between 20 40))) emptyFilter
    , addPositiveCondition (Condition_Interest (InterestCondition (Interest.Between 9.99 19.99))) emptyFilter
    , addPositiveCondition (Condition_Rating (WorseThan A_Plus)) <| addPositiveCondition (Condition_Income (MainIncomeList [ UNEMPLOYED, PENSION ])) emptyFilter
    , addPositiveCondition (Condition_Region (RegionList [ USTECKY, MORAVSKOSLEZSKY ])) <| addPositiveCondition (Condition_Term (TermCondition (MoreThan 36))) <| addNegativeCondition (Condition_Rating (BetterThan A_Plus)) emptyFilter
    , setFilteredItem Participation <| addPositiveCondition (Condition_Term (TermCondition (MoreThan 36))) emptyFilter
    , setFilteredItem Loan_And_Participation <| addPositiveCondition (Condition_Term (TermCondition (MoreThan 36))) emptyFilter
    ]
