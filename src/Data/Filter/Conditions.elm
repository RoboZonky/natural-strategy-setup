module Data.Filter.Conditions
    exposing
        ( Condition(..)
        , Conditions
        , addCondition
        , conditionsToList
        , conditionsValidationErrors
        , emptyConditions
        , removeAmountCondition
        , removeInterestCondition
        , removeLoanTermCondition
        , removeMainIncomeCondition
        , removePurposeCondition
        , removeRatingCondition
        , removeRegionCondition
        , removeStoryCondition
        , renderConditionList
        , updateAmount
        , updateInterest
        , updateLoanTerm
        , updateMainIncome
        , updatePurpose
        , updateRating
        , updateRegion
        , updateStory
        )

import Data.Filter.Conditions.Amount as Amount exposing (AmountCondition, AmountMsg, renderAmountCondition)
import Data.Filter.Conditions.Interest as Interest exposing (InterestCondition, InterestMsg, renderInterestCondition)
import Data.Filter.Conditions.LoanPurpose as LoanPurpose exposing (LoanPurposeCondition, LoanPurposeMsg, renderLoanPurposeCondition)
import Data.Filter.Conditions.LoanTerm as LoanTerm exposing (LoanTermCondition, LoanTermMsg, renderLoanTermCondition)
import Data.Filter.Conditions.MainIncome as MainIncome exposing (MainIncomeCondition, MainIncomeMsg, renderMainIncomeCondition)
import Data.Filter.Conditions.Rating as Rating exposing (RatingCondition, RatingMsg, renderRatingCondition)
import Data.Filter.Conditions.Region as Region exposing (RegionCondition, RegionMsg, renderRegionCondition)
import Data.Filter.Conditions.Story as Story exposing (StoryCondition, StoryMsg, renderStoryCondition)


type alias Conditions =
    { region : Maybe RegionCondition
    , rating : Maybe RatingCondition
    , income : Maybe MainIncomeCondition
    , purpose : Maybe LoanPurposeCondition
    , story : Maybe StoryCondition
    , term : Maybe LoanTermCondition
    , amount : Maybe AmountCondition
    , interest : Maybe InterestCondition
    }


type Condition
    = Condition_Region RegionCondition
    | Condition_Rating RatingCondition
    | Condition_Income MainIncomeCondition
    | Condition_Purpose LoanPurposeCondition
    | Condition_Story StoryCondition
    | Condition_Term LoanTermCondition
    | Condition_Amount AmountCondition
    | Condition_Interest InterestCondition


emptyConditions : Conditions
emptyConditions =
    { region = Nothing
    , rating = Nothing
    , income = Nothing
    , purpose = Nothing
    , story = Nothing
    , term = Nothing
    , amount = Nothing
    , interest = Nothing
    }


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


renderCondition : Condition -> String
renderCondition condition =
    case condition of
        Condition_Region regionCond ->
            renderRegionCondition regionCond

        Condition_Rating ratingCond ->
            renderRatingCondition ratingCond

        Condition_Income incomeCond ->
            renderMainIncomeCondition incomeCond

        Condition_Purpose loanPurposeCond ->
            renderLoanPurposeCondition loanPurposeCond

        Condition_Story storyCond ->
            renderStoryCondition storyCond

        Condition_Term termCond ->
            renderLoanTermCondition termCond

        Condition_Amount amountCond ->
            renderAmountCondition amountCond

        Condition_Interest interestCond ->
            renderInterestCondition interestCond


conditionsValidationErrors : Conditions -> List String
conditionsValidationErrors =
    List.concat << List.map conditionValidationError << conditionsToList


conditionValidationError : Condition -> List String
conditionValidationError c =
    case c of
        Condition_Region regionCond ->
            Region.validationErrors regionCond

        Condition_Rating ratingCond ->
            Rating.validationErrors ratingCond

        Condition_Income incomeCond ->
            MainIncome.validationErrors incomeCond

        Condition_Purpose loanPurposeCond ->
            LoanPurpose.validationErrors loanPurposeCond

        Condition_Story storyCond ->
            [{- Story condition can't be invalid -> valid. errors list always empty -}]

        Condition_Term termCond ->
            LoanTerm.validationErrors termCond

        Condition_Amount amountCond ->
            Amount.validationErrors amountCond

        Condition_Interest interestCond ->
            Interest.validationErrors interestCond


conditionsToList : Conditions -> List Condition
conditionsToList { region, rating, income, purpose, story, term, amount, interest } =
    let
        fromMaybe : (a -> Condition) -> Maybe a -> List Condition
        fromMaybe wrap =
            Maybe.withDefault [] << Maybe.map (List.singleton << wrap)

        reg =
            fromMaybe Condition_Region region

        rat =
            fromMaybe Condition_Rating rating

        inc =
            fromMaybe Condition_Income income

        pur =
            fromMaybe Condition_Purpose purpose

        sto =
            fromMaybe Condition_Story story

        ter =
            fromMaybe Condition_Term term

        amo =
            fromMaybe Condition_Amount amount

        inte =
            fromMaybe Condition_Interest interest
    in
    List.concat [ reg, rat, inc, pur, sto, ter, amo, inte ]


addCondition : Condition -> Conditions -> Conditions
addCondition c cs =
    case c of
        Condition_Region regionCond ->
            setRegionCondition regionCond cs

        Condition_Rating ratingCond ->
            setRatingCondition ratingCond cs

        Condition_Income incomeCond ->
            setIncomeCondition incomeCond cs

        Condition_Purpose loanPurposeCond ->
            setLoanPurposeCondition loanPurposeCond cs

        Condition_Story storyCond ->
            setStoryCondition storyCond cs

        Condition_Term termCond ->
            setTermCondition termCond cs

        Condition_Amount amountCond ->
            setAmountCondition amountCond cs

        Condition_Interest interestCond ->
            setInterestCondition interestCond cs


setRegionCondition : RegionCondition -> Conditions -> Conditions
setRegionCondition c cs =
    { cs | region = Just c }


setRatingCondition : RatingCondition -> Conditions -> Conditions
setRatingCondition c cs =
    { cs | rating = Just c }


setIncomeCondition : MainIncomeCondition -> Conditions -> Conditions
setIncomeCondition c cs =
    { cs | income = Just c }


setLoanPurposeCondition : LoanPurposeCondition -> Conditions -> Conditions
setLoanPurposeCondition c cs =
    { cs | purpose = Just c }


setStoryCondition : StoryCondition -> Conditions -> Conditions
setStoryCondition c cs =
    { cs | story = Just c }


setTermCondition : LoanTermCondition -> Conditions -> Conditions
setTermCondition c cs =
    { cs | term = Just c }


setAmountCondition : AmountCondition -> Conditions -> Conditions
setAmountCondition c cs =
    { cs | amount = Just c }


setInterestCondition : InterestCondition -> Conditions -> Conditions
setInterestCondition c cs =
    { cs | interest = Just c }


updateInterest : InterestMsg -> Conditions -> Conditions
updateInterest msg conditions =
    { conditions | interest = Maybe.map (Interest.update msg) conditions.interest }


updateAmount : AmountMsg -> Conditions -> Conditions
updateAmount msg conditions =
    { conditions | amount = Maybe.map (Amount.update msg) conditions.amount }


updateStory : StoryMsg -> Conditions -> Conditions
updateStory msg conditions =
    { conditions | story = Maybe.map (Story.update msg) conditions.story }


updatePurpose : LoanPurposeMsg -> Conditions -> Conditions
updatePurpose msg conditions =
    { conditions | purpose = Maybe.map (LoanPurpose.update msg) conditions.purpose }


updateLoanTerm : LoanTermMsg -> Conditions -> Conditions
updateLoanTerm msg conditions =
    { conditions | term = Maybe.map (LoanTerm.update msg) conditions.term }


updateMainIncome : MainIncomeMsg -> Conditions -> Conditions
updateMainIncome msg conditions =
    { conditions | income = Maybe.map (MainIncome.update msg) conditions.income }


updateRating : RatingMsg -> Conditions -> Conditions
updateRating msg conditions =
    { conditions | rating = Maybe.map (Rating.update msg) conditions.rating }


updateRegion : RegionMsg -> Conditions -> Conditions
updateRegion msg conditions =
    { conditions | region = Maybe.map (Region.update msg) conditions.region }


removeAmountCondition : Conditions -> Conditions
removeAmountCondition cs =
    { cs | amount = Nothing }


removeStoryCondition : Conditions -> Conditions
removeStoryCondition cs =
    { cs | story = Nothing }


removeInterestCondition : Conditions -> Conditions
removeInterestCondition cs =
    { cs | interest = Nothing }


removePurposeCondition : Conditions -> Conditions
removePurposeCondition cs =
    { cs | purpose = Nothing }


removeLoanTermCondition : Conditions -> Conditions
removeLoanTermCondition cs =
    { cs | term = Nothing }


removeMainIncomeCondition : Conditions -> Conditions
removeMainIncomeCondition cs =
    { cs | income = Nothing }


removeRatingCondition : Conditions -> Conditions
removeRatingCondition cs =
    { cs | rating = Nothing }


removeRegionCondition : Conditions -> Conditions
removeRegionCondition cs =
    { cs | region = Nothing }
