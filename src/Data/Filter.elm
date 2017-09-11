module Data.Filter
    exposing
        ( Condition(..)
        , Conditions
        , FilteredItem(..)
        , MarketplaceFilter(..)
        , addNegativeCondition
        , addPositiveCondition
        , emptyConditions
        , emptyFilter
        , filtereedItemFromString
        , isValid
        , marketplaceFilterValidationErrors
        , removePositiveAmountCondition
        , removePositiveIncomeCondition
        , removePositiveInterestCondition
        , removePositivePurposeCondition
        , removePositiveRatingCondition
        , removePositiveRegionCondition
        , removePositiveStoryCondition
        , removePositiveTermCondition
        , renderFilteredItem
        , renderFilters
        , renderMarketplaceFilter
        , setFilteredItem
        )

import Data.Filter.Condition.Amount as Amount exposing (AmountCondition, renderAmountCondition)
import Data.Filter.Condition.Interest as Interest exposing (InterestCondition, renderInterestCondition)
import Data.Filter.Condition.LoanPurpose as LoanPurpose exposing (LoanPurposeCondition, renderLoanPurposeCondition)
import Data.Filter.Condition.LoanTerm as LoanTerm exposing (TermCondition, renderTermCondition)
import Data.Filter.Condition.MainIncome as MainIncome exposing (MainIncomeCondition, renderIncomeCondition)
import Data.Filter.Condition.Rating as Rating exposing (RatingCondition, renderRatingCondition)
import Data.Filter.Condition.Region as Region exposing (RegionCondition, renderRegionCondition)
import Data.Filter.Condition.Story exposing (StoryCondition, renderStoryCondition)
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
        , ignoreWhen : Conditions
        , butNotWhen : Conditions
        }


emptyFilter : MarketplaceFilter
emptyFilter =
    MarketplaceFilter
        { whatToFilter = Loan
        , ignoreWhen = emptyConditions
        , butNotWhen = emptyConditions
        }


isValid : MarketplaceFilter -> Bool
isValid =
    List.isEmpty << marketplaceFilterValidationErrors


marketplaceFilterValidationErrors : MarketplaceFilter -> List String
marketplaceFilterValidationErrors (MarketplaceFilter mf) =
    let
        atLeastOnePositiveCondition =
            Util.validate (List.isEmpty <| conditionsToList mf.ignoreWhen) "Filtr musí obsahovat aspoň jednu podmínku"
    in
    atLeastOnePositiveCondition ++ conditionsValidationErrors mf.ignoreWhen


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


setFilteredItem : FilteredItem -> MarketplaceFilter -> MarketplaceFilter
setFilteredItem newItem (MarketplaceFilter mf) =
    MarketplaceFilter { mf | whatToFilter = newItem }


addPositiveCondition : Condition -> MarketplaceFilter -> MarketplaceFilter
addPositiveCondition c (MarketplaceFilter mf) =
    MarketplaceFilter { mf | ignoreWhen = addCondition c mf.ignoreWhen }


addNegativeCondition : Condition -> MarketplaceFilter -> MarketplaceFilter
addNegativeCondition c (MarketplaceFilter mf) =
    MarketplaceFilter { mf | butNotWhen = addCondition c mf.butNotWhen }


renderMarketplaceFilter : MarketplaceFilter -> String
renderMarketplaceFilter (MarketplaceFilter { whatToFilter, ignoreWhen, butNotWhen }) =
    let
        negativePart =
            if List.isEmpty (conditionsToList butNotWhen) then
                ""
            else
                "\n(Ale ne když: " ++ renderConditionList (conditionsToList butNotWhen) ++ ")"

        positivePart =
            renderConditionList <| conditionsToList ignoreWhen
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
    | Condition_Income MainIncomeCondition
    | Condition_Purpose LoanPurposeCondition
    | Condition_Story StoryCondition
    | Condition_Term TermCondition
    | Condition_Amount AmountCondition
    | Condition_Interest InterestCondition


type alias Conditions =
    { region : Maybe RegionCondition
    , rating : Maybe RatingCondition
    , income : Maybe MainIncomeCondition
    , purpose : Maybe LoanPurposeCondition
    , story : Maybe StoryCondition
    , term : Maybe TermCondition
    , amount : Maybe AmountCondition
    , interest : Maybe InterestCondition
    }


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


setTermCondition : TermCondition -> Conditions -> Conditions
setTermCondition c cs =
    { cs | term = Just c }


setAmountCondition : AmountCondition -> Conditions -> Conditions
setAmountCondition c cs =
    { cs | amount = Just c }


setInterestCondition : InterestCondition -> Conditions -> Conditions
setInterestCondition c cs =
    { cs | interest = Just c }


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


removeTermCondition : Conditions -> Conditions
removeTermCondition cs =
    { cs | term = Nothing }


removeIncomeCondition : Conditions -> Conditions
removeIncomeCondition cs =
    { cs | income = Nothing }


removeRatingCondition : Conditions -> Conditions
removeRatingCondition cs =
    { cs | rating = Nothing }


removeRegionCondition : Conditions -> Conditions
removeRegionCondition cs =
    { cs | region = Nothing }


removePositiveAmountCondition : MarketplaceFilter -> MarketplaceFilter
removePositiveAmountCondition =
    modifyPositiveConditions removeAmountCondition


removePositiveStoryCondition : MarketplaceFilter -> MarketplaceFilter
removePositiveStoryCondition =
    modifyPositiveConditions removeStoryCondition


removePositiveInterestCondition : MarketplaceFilter -> MarketplaceFilter
removePositiveInterestCondition =
    modifyPositiveConditions removeInterestCondition


removePositivePurposeCondition : MarketplaceFilter -> MarketplaceFilter
removePositivePurposeCondition =
    modifyPositiveConditions removePurposeCondition


removePositiveTermCondition : MarketplaceFilter -> MarketplaceFilter
removePositiveTermCondition =
    modifyPositiveConditions removeTermCondition


removePositiveIncomeCondition : MarketplaceFilter -> MarketplaceFilter
removePositiveIncomeCondition =
    modifyPositiveConditions removeIncomeCondition


removePositiveRatingCondition : MarketplaceFilter -> MarketplaceFilter
removePositiveRatingCondition =
    modifyPositiveConditions removeRatingCondition


removePositiveRegionCondition : MarketplaceFilter -> MarketplaceFilter
removePositiveRegionCondition =
    modifyPositiveConditions removeRegionCondition


modifyPositiveConditions : (Conditions -> Conditions) -> MarketplaceFilter -> MarketplaceFilter
modifyPositiveConditions updater (MarketplaceFilter mf) =
    MarketplaceFilter { mf | ignoreWhen = updater mf.ignoreWhen }


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
