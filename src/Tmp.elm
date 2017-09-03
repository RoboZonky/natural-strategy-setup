module Tmp exposing (..)

import Data.Filter exposing (..)
import Data.Filter.Condition.Interest as Interest exposing (InterestCondition(..))
import Data.Filter.Condition.LoanPurpose exposing (..)
import Data.Filter.Condition.LoanTerm exposing (..)
import Data.Filter.Condition.MainIncome exposing (IncomeCondition(..), MainIncome(..))
import Data.Filter.Condition.Region exposing (..)
import Data.Filter.Condition.Story exposing (..)
import Data.Rating exposing (..)


simpleFilter : Condition -> MarketplaceFilter
simpleFilter c =
    MarketplaceFilter
        { whatToFilter = Loan
        , ignoreWhen = [ c ]
        , butNotWhen = []
        }


sampleFilters : List MarketplaceFilter
sampleFilters =
    [ simpleFilter <| Condition_Region (RegionList [ USTECKY ])
    , simpleFilter <| Condition_Purpose (LoanPurposeList [ CESTOVANI, JINE ])
    , simpleFilter <| Condition_Income (IncomeList [ STUDENT, UNEMPLOYED ])
    , simpleFilter <| Condition_Rating (RatingList [ A ])
    , simpleFilter <| Condition_Rating (RatingList [ C, D ])
    , simpleFilter <| Condition_Rating (RatingList [ A, B, C ])
    , simpleFilter <| Condition_Rating (WorseThan A)
    , simpleFilter <| Condition_Rating (BetterThan A_Plus)
    , simpleFilter <| Condition_Story (StoryCondition SHORT)
    , simpleFilter <| Condition_Term (TermCondition (LessThan 36))
    , simpleFilter <| Condition_Term (TermCondition (Between 20 40))
    , simpleFilter <| Condition_Interest (InterestCondition (Interest.Between 9.99 19.99))
    , MarketplaceFilter
        { whatToFilter = Loan
        , ignoreWhen = [ Condition_Rating (WorseThan A_Plus), Condition_Income (IncomeList [ UNEMPLOYED, PENSION ]) ]
        , butNotWhen = []
        }
    , MarketplaceFilter
        { whatToFilter = Loan
        , ignoreWhen = [ Condition_Region (RegionList [ USTECKY, MORAVSKOSLEZSKY ]), Condition_Term (TermCondition (MoreThan 36)) ]
        , butNotWhen = [ Condition_Rating (BetterThan A_Plus) ]
        }
    , MarketplaceFilter
        { whatToFilter = Participation
        , ignoreWhen = [ Condition_Term (TermCondition (MoreThan 36)) ]
        , butNotWhen = []
        }
    , MarketplaceFilter
        { whatToFilter = Loan_And_Participation
        , ignoreWhen = [ Condition_Term (TermCondition (MoreThan 36)) ]
        , butNotWhen = []
        }
    ]
