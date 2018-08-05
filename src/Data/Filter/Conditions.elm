module Data.Filter.Conditions
    exposing
        ( Condition(..)
        , ConditionType(..)
        , Conditions
        , addCondition
        , conditionsDecoder
        , conditionsValidationErrors
        , emptyConditions
        , encodeConditions
        , getDefaultCondition
        , getDisabledConditions
        , getEnabledConditions
        , removeCondition
        , renderCondition
        , updateAmount
        , updateElapsedTermMonths
        , updateElapsedTermPercent
        , updateIncome
        , updateInsurance
        , updateInterest
        , updatePurpose
        , updateRating
        , updateRegion
        , updateStory
        , updateTermMonths
        , updateTermPercent
        )

import Data.Filter.Conditions.Amount as Amount exposing (AmountCondition, AmountMsg)
import Data.Filter.Conditions.ElapsedTermMonths as ElapsedTermMonths exposing (ElapsedTermMonthsCondition, ElapsedTermMonthsMsg)
import Data.Filter.Conditions.ElapsedTermPercent as ElapsedTermPercent exposing (ElapsedTermPercentCondition, ElapsedTermPercentMsg)
import Data.Filter.Conditions.Income as Income exposing (IncomeCondition, IncomeMsg)
import Data.Filter.Conditions.Insurance as Insurance exposing (InsuranceCondition, InsuranceMsg)
import Data.Filter.Conditions.Interest as Interest exposing (InterestCondition, InterestMsg)
import Data.Filter.Conditions.Purpose as Purpose exposing (PurposeCondition, PurposeMsg)
import Data.Filter.Conditions.Rating as Rating exposing (RatingCondition, RatingMsg)
import Data.Filter.Conditions.Region as Region exposing (RegionCondition, RegionMsg)
import Data.Filter.Conditions.Story as Story exposing (StoryCondition, StoryMsg)
import Data.Filter.Conditions.TermMonths as TermMonths exposing (TermMonthsCondition, TermMonthsMsg)
import Data.Filter.Conditions.TermPercent as TermPercent exposing (TermPercentCondition, TermPercentMsg)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra exposing ((|:), optionalField)
import Json.Encode as Encode exposing (Value)
import List


type alias Conditions =
    { region : Maybe RegionCondition
    , rating : Maybe RatingCondition
    , income : Maybe IncomeCondition
    , purpose : Maybe PurposeCondition
    , story : Maybe StoryCondition
    , termMonths : Maybe TermMonthsCondition
    , termPercent : Maybe TermPercentCondition
    , elapsedTermMonths : Maybe ElapsedTermMonthsCondition
    , elapsedTermPercent : Maybe ElapsedTermPercentCondition
    , interest : Maybe InterestCondition
    , amount : Maybe AmountCondition
    , insurance : Maybe InsuranceCondition
    }


type Condition
    = Condition_Amount AmountCondition
    | Condition_Elapsed_Term_Months ElapsedTermMonthsCondition
    | Condition_Elapsed_Term_Percent ElapsedTermPercentCondition
    | Condition_Income IncomeCondition
    | Condition_Insurance InsuranceCondition
    | Condition_Interest InterestCondition
    | Condition_Purpose PurposeCondition
    | Condition_Rating RatingCondition
    | Condition_Region RegionCondition
    | Condition_Story StoryCondition
    | Condition_Term_Months TermMonthsCondition
    | Condition_Term_Percent TermPercentCondition


type ConditionType
    = Amount
    | Elapsed_Term_Months
    | Elapsed_Term_Percent
    | Income
    | Insurance
    | Interest
    | Purpose
    | Rating
    | Region
    | Story
    | Term_Months
    | Term_Percent


emptyConditions : Conditions
emptyConditions =
    { region = Nothing
    , rating = Nothing
    , income = Nothing
    , purpose = Nothing
    , story = Nothing
    , termMonths = Nothing
    , termPercent = Nothing
    , elapsedTermMonths = Nothing
    , elapsedTermPercent = Nothing
    , interest = Nothing
    , amount = Nothing
    , insurance = Nothing
    }


renderCondition : Condition -> String
renderCondition condition =
    case condition of
        Condition_Region c ->
            Region.renderCondition c

        Condition_Rating c ->
            Rating.renderCondition c

        Condition_Income c ->
            Income.renderCondition c

        Condition_Purpose c ->
            Purpose.renderCondition c

        Condition_Story c ->
            Story.renderCondition c

        Condition_Term_Months c ->
            TermMonths.renderCondition c

        Condition_Term_Percent c ->
            TermPercent.renderCondition c

        Condition_Elapsed_Term_Months c ->
            ElapsedTermMonths.renderCondition c

        Condition_Elapsed_Term_Percent c ->
            ElapsedTermPercent.renderCondition c

        Condition_Interest c ->
            Interest.renderCondition c

        Condition_Amount c ->
            Amount.renderCondition c

        Condition_Insurance c ->
            Insurance.renderCondition c


conditionsValidationErrors : String -> Conditions -> List String
conditionsValidationErrors errorPrefix =
    List.map (\e -> errorPrefix ++ e) << List.concat << List.map conditionValidationError << getEnabledConditions


conditionValidationError : Condition -> List String
conditionValidationError c =
    case c of
        Condition_Region regionCond ->
            Region.validationErrors regionCond

        Condition_Rating ratingCond ->
            Rating.validationErrors ratingCond

        Condition_Income incomeCond ->
            Income.validationErrors incomeCond

        Condition_Purpose purposeCond ->
            Purpose.validationErrors purposeCond

        Condition_Story _ ->
            [{- Story condition can't be invalid -> valid. errors list always empty -}]

        Condition_Term_Months termMonthsCond ->
            TermMonths.validationErrors termMonthsCond

        Condition_Term_Percent termPercentCond ->
            TermPercent.validationErrors termPercentCond

        Condition_Elapsed_Term_Months elapsedTermMonthsCond ->
            ElapsedTermMonths.validationErrors elapsedTermMonthsCond

        Condition_Elapsed_Term_Percent elapsedTermPercentCond ->
            ElapsedTermPercent.validationErrors elapsedTermPercentCond

        Condition_Interest interestCond ->
            Interest.validationErrors interestCond

        Condition_Amount amountCond ->
            Amount.validationErrors amountCond

        Condition_Insurance _ ->
            [{- Insurance condition can't be invalid -> valid. errors list always empty -}]


getDisabledConditions : Conditions -> List ConditionType
getDisabledConditions cs =
    let
        addIfNothing : (Conditions -> Maybe a) -> ConditionType -> List ConditionType
        addIfNothing field conditionType =
            Maybe.withDefault [ conditionType ] <| Maybe.map (always []) <| field cs
    in
    List.concat
        [ addIfNothing .amount Amount
        , addIfNothing .elapsedTermMonths Elapsed_Term_Months
        , addIfNothing .elapsedTermPercent Elapsed_Term_Percent
        , addIfNothing .income Income
        , addIfNothing .insurance Insurance
        , addIfNothing .interest Interest
        , addIfNothing .purpose Purpose
        , addIfNothing .rating Rating
        , addIfNothing .region Region
        , addIfNothing .story Story
        , addIfNothing .termMonths Term_Months
        , addIfNothing .termPercent Term_Percent
        ]


getEnabledConditions : Conditions -> List Condition
getEnabledConditions cs =
    let
        addIfJust : (Conditions -> Maybe a) -> (a -> Condition) -> List Condition
        addIfJust field wrap =
            Maybe.withDefault [] <| Maybe.map (List.singleton << wrap) <| field cs
    in
    List.concat
        [ addIfJust .amount Condition_Amount
        , addIfJust .elapsedTermMonths Condition_Elapsed_Term_Months
        , addIfJust .elapsedTermPercent Condition_Elapsed_Term_Percent
        , addIfJust .income Condition_Income
        , addIfJust .insurance Condition_Insurance
        , addIfJust .interest Condition_Interest
        , addIfJust .purpose Condition_Purpose
        , addIfJust .rating Condition_Rating
        , addIfJust .region Condition_Region
        , addIfJust .story Condition_Story
        , addIfJust .termMonths Condition_Term_Months
        , addIfJust .termPercent Condition_Term_Percent
        ]


addCondition : Condition -> Conditions -> Conditions
addCondition c cs =
    case c of
        Condition_Amount amountCond ->
            setAmountCondition amountCond cs

        Condition_Elapsed_Term_Months elapsedTermMonthsCond ->
            setElapsedTermMonthsCondition elapsedTermMonthsCond cs

        Condition_Elapsed_Term_Percent elapsedTermPercentCond ->
            setElapsedTermPercentCondition elapsedTermPercentCond cs

        Condition_Income incomeCond ->
            setIncomeCondition incomeCond cs

        Condition_Insurance insuraceCond ->
            setInsuranceCondition insuraceCond cs

        Condition_Interest interestCond ->
            setInterestCondition interestCond cs

        Condition_Purpose purposeCond ->
            setPurposeCondition purposeCond cs

        Condition_Rating ratingCond ->
            setRatingCondition ratingCond cs

        Condition_Region regionCond ->
            setRegionCondition regionCond cs

        Condition_Story storyCond ->
            setStoryCondition storyCond cs

        Condition_Term_Months termMonthsCond ->
            setTermMonthsCondition termMonthsCond cs

        Condition_Term_Percent termPercentCond ->
            setTermPercentCondition termPercentCond cs


setRegionCondition : RegionCondition -> Conditions -> Conditions
setRegionCondition c cs =
    { cs | region = Just c }


setRatingCondition : RatingCondition -> Conditions -> Conditions
setRatingCondition c cs =
    { cs | rating = Just c }


setIncomeCondition : IncomeCondition -> Conditions -> Conditions
setIncomeCondition c cs =
    { cs | income = Just c }


setPurposeCondition : PurposeCondition -> Conditions -> Conditions
setPurposeCondition c cs =
    { cs | purpose = Just c }


setStoryCondition : StoryCondition -> Conditions -> Conditions
setStoryCondition c cs =
    { cs | story = Just c }


setTermMonthsCondition : TermMonthsCondition -> Conditions -> Conditions
setTermMonthsCondition c cs =
    { cs | termMonths = Just c }


setTermPercentCondition : TermPercentCondition -> Conditions -> Conditions
setTermPercentCondition c cs =
    { cs | termPercent = Just c }


setElapsedTermMonthsCondition : ElapsedTermMonthsCondition -> Conditions -> Conditions
setElapsedTermMonthsCondition c cs =
    { cs | elapsedTermMonths = Just c }


setElapsedTermPercentCondition : ElapsedTermPercentCondition -> Conditions -> Conditions
setElapsedTermPercentCondition c cs =
    { cs | elapsedTermPercent = Just c }


setInterestCondition : InterestCondition -> Conditions -> Conditions
setInterestCondition c cs =
    { cs | interest = Just c }


setAmountCondition : AmountCondition -> Conditions -> Conditions
setAmountCondition c cs =
    { cs | amount = Just c }


setInsuranceCondition : InsuranceCondition -> Conditions -> Conditions
setInsuranceCondition c cs =
    { cs | insurance = Just c }


updateInterest : InterestMsg -> Conditions -> Conditions
updateInterest msg conditions =
    { conditions | interest = Maybe.map (Interest.update msg) conditions.interest }


updateAmount : AmountMsg -> Conditions -> Conditions
updateAmount msg conditions =
    { conditions | amount = Maybe.map (Amount.update msg) conditions.amount }


updateStory : StoryMsg -> Conditions -> Conditions
updateStory msg conditions =
    { conditions | story = Maybe.map (Story.update msg) conditions.story }


updatePurpose : PurposeMsg -> Conditions -> Conditions
updatePurpose msg conditions =
    { conditions | purpose = Maybe.map (Purpose.update msg) conditions.purpose }


updateTermMonths : TermMonthsMsg -> Conditions -> Conditions
updateTermMonths msg conditions =
    { conditions | termMonths = Maybe.map (TermMonths.update msg) conditions.termMonths }


updateTermPercent : TermPercentMsg -> Conditions -> Conditions
updateTermPercent msg conditions =
    { conditions | termPercent = Maybe.map (TermPercent.update msg) conditions.termPercent }


updateElapsedTermMonths : ElapsedTermMonthsMsg -> Conditions -> Conditions
updateElapsedTermMonths msg conditions =
    { conditions | elapsedTermMonths = Maybe.map (ElapsedTermMonths.update msg) conditions.elapsedTermMonths }


updateElapsedTermPercent : ElapsedTermPercentMsg -> Conditions -> Conditions
updateElapsedTermPercent msg conditions =
    { conditions | elapsedTermPercent = Maybe.map (ElapsedTermPercent.update msg) conditions.elapsedTermPercent }


updateIncome : IncomeMsg -> Conditions -> Conditions
updateIncome msg conditions =
    { conditions | income = Maybe.map (Income.update msg) conditions.income }


updateRating : RatingMsg -> Conditions -> Conditions
updateRating msg conditions =
    { conditions | rating = Maybe.map (Rating.update msg) conditions.rating }


updateRegion : RegionMsg -> Conditions -> Conditions
updateRegion msg conditions =
    { conditions | region = Maybe.map (Region.update msg) conditions.region }


updateInsurance : InsuranceMsg -> Conditions -> Conditions
updateInsurance msg conditions =
    { conditions | insurance = Maybe.map (Insurance.update msg) conditions.insurance }


removeCondition : ConditionType -> Conditions -> Conditions
removeCondition conditionType cs =
    case conditionType of
        Amount ->
            { cs | amount = Nothing }

        Elapsed_Term_Months ->
            { cs | elapsedTermMonths = Nothing }

        Elapsed_Term_Percent ->
            { cs | elapsedTermPercent = Nothing }

        Income ->
            { cs | income = Nothing }

        Insurance ->
            { cs | insurance = Nothing }

        Interest ->
            { cs | interest = Nothing }

        Purpose ->
            { cs | purpose = Nothing }

        Rating ->
            { cs | rating = Nothing }

        Region ->
            { cs | region = Nothing }

        Story ->
            { cs | story = Nothing }

        Term_Months ->
            { cs | termMonths = Nothing }

        Term_Percent ->
            { cs | termPercent = Nothing }


getDefaultCondition : ConditionType -> Condition
getDefaultCondition conditionType =
    case conditionType of
        Amount ->
            Condition_Amount Amount.defaultCondition

        Elapsed_Term_Months ->
            Condition_Elapsed_Term_Months ElapsedTermMonths.defaultCondition

        Elapsed_Term_Percent ->
            Condition_Elapsed_Term_Percent ElapsedTermPercent.defaultCondition

        Income ->
            Condition_Income Income.defaultCondition

        Insurance ->
            Condition_Insurance Insurance.defaultCondition

        Interest ->
            Condition_Interest Interest.defaultCondition

        Purpose ->
            Condition_Purpose Purpose.defaultCondition

        Rating ->
            Condition_Rating Rating.defaultCondition

        Region ->
            Condition_Region Region.defaultCondition

        Story ->
            Condition_Story Story.defaultCondition

        Term_Months ->
            Condition_Term_Months TermMonths.defaultCondition

        Term_Percent ->
            Condition_Term_Percent TermPercent.defaultCondition



-- JSON


encodeConditions : Conditions -> Value
encodeConditions conditions =
    getEnabledConditions conditions
        |> List.map encodeCondition
        |> Encode.object


encodeCondition : Condition -> ( String, Value )
encodeCondition c =
    case c of
        Condition_Region regionCond ->
            ( "A", Region.encodeCondition regionCond )

        Condition_Rating ratingCond ->
            ( "B", Rating.encodeCondition ratingCond )

        Condition_Income incomeCond ->
            ( "C", Income.encodeCondition incomeCond )

        Condition_Purpose purposeCond ->
            ( "D", Purpose.encodeCondition purposeCond )

        Condition_Story storyCond ->
            ( "E", Story.encodeCondition storyCond )

        Condition_Term_Months termMonthsCond ->
            ( "F", TermMonths.encodeCondition termMonthsCond )

        Condition_Term_Percent termPercentCond ->
            ( "G", TermPercent.encodeCondition termPercentCond )

        Condition_Elapsed_Term_Months elapsedTermMonthsCond ->
            ( "H", ElapsedTermMonths.encodeCondition elapsedTermMonthsCond )

        Condition_Elapsed_Term_Percent elapsedTermPercentCond ->
            ( "I", ElapsedTermPercent.encodeCondition elapsedTermPercentCond )

        Condition_Interest interestCond ->
            ( "J", Interest.encodeCondition interestCond )

        Condition_Amount amountCond ->
            ( "K", Amount.encodeCondition amountCond )

        Condition_Insurance insuranceCond ->
            ( "L", Insurance.encodeCondition insuranceCond )


conditionsDecoder : Decoder Conditions
conditionsDecoder =
    Decode.succeed Conditions
        |: optionalField "A" Region.conditionDecoder
        |: optionalField "B" Rating.conditionDecoder
        |: optionalField "C" Income.conditionDecoder
        |: optionalField "D" Purpose.conditionDecoder
        |: optionalField "E" Story.conditionDecoder
        |: optionalField "F" TermMonths.conditionDecoder
        |: optionalField "G" TermPercent.conditionDecoder
        |: optionalField "H" ElapsedTermMonths.conditionDecoder
        |: optionalField "I" ElapsedTermPercent.conditionDecoder
        |: optionalField "J" Interest.conditionDecoder
        |: optionalField "K" Amount.conditionDecoder
        |: optionalField "L" Insurance.conditionDecoder
