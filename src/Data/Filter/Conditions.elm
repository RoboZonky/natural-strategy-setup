module Data.Filter.Conditions
    exposing
        ( Condition(..)
        , Conditions
        , addCondition
        , conditionsDecoder
        , conditionsToList
        , conditionsValidationErrors
        , emptyConditions
        , encodeConditions
        , removeAmountCondition
        , removeElapsedTermMonthsCondition
        , removeElapsedTermPercentCondition
        , removeInterestCondition
        , removeMainIncomeCondition
        , removePurposeCondition
        , removeRatingCondition
        , removeRegionCondition
        , removeStoryCondition
        , removeTermMonthsCondition
        , removeTermPercentCondition
        , renderConditionList
        , updateAmount
        , updateElapsedTermMonths
        , updateElapsedTermPercent
        , updateInterest
        , updateMainIncome
        , updatePurpose
        , updateRating
        , updateRegion
        , updateStory
        , updateTermMonths
        , updateTermPercent
        )

import Data.Filter.Conditions.Amount as Amount exposing (AmountCondition, AmountMsg, renderAmountCondition)
import Data.Filter.Conditions.ElapsedTermMonths as ElapsedTermMonths exposing (ElapsedTermMonthsCondition, ElapsedTermMonthsMsg, renderElapsedTermMonthsCondition)
import Data.Filter.Conditions.ElapsedTermPercent as ElapsedTermPercent exposing (ElapsedTermPercentCondition, ElapsedTermPercentMsg, renderElapsedTermPercentCondition)
import Data.Filter.Conditions.Interest as Interest exposing (InterestCondition, InterestMsg, renderInterestCondition)
import Data.Filter.Conditions.MainIncome as MainIncome exposing (MainIncomeCondition, MainIncomeMsg, renderMainIncomeCondition)
import Data.Filter.Conditions.Purpose as Purpose exposing (PurposeCondition, PurposeMsg, renderPurposeCondition)
import Data.Filter.Conditions.Rating as Rating exposing (RatingCondition, RatingMsg, renderRatingCondition)
import Data.Filter.Conditions.Region as Region exposing (RegionCondition, RegionMsg, renderRegionCondition)
import Data.Filter.Conditions.Story as Story exposing (StoryCondition, StoryMsg, renderStoryCondition)
import Data.Filter.Conditions.TermMonths as TermMonths exposing (TermMonthsCondition, TermMonthsMsg, renderTermMonthsCondition)
import Data.Filter.Conditions.TermPercent as TermPercent exposing (TermPercentCondition, TermPercentMsg, renderTermPercentCondition)
import Json.Decode as Decode exposing (Decoder, field, nullable)
import Json.Decode.Extra exposing ((|:))
import Json.Encode as Encode exposing (Value)


type alias Conditions =
    { region : Maybe RegionCondition
    , rating : Maybe RatingCondition
    , income : Maybe MainIncomeCondition
    , purpose : Maybe PurposeCondition
    , story : Maybe StoryCondition
    , termMonths : Maybe TermMonthsCondition
    , termPercent : Maybe TermPercentCondition
    , elapsedTermMonths : Maybe ElapsedTermMonthsCondition
    , elapsedTermPercent : Maybe ElapsedTermPercentCondition
    , interest : Maybe InterestCondition
    , amount : Maybe AmountCondition
    }


type Condition
    = Condition_Region RegionCondition
    | Condition_Rating RatingCondition
    | Condition_Income MainIncomeCondition
    | Condition_Purpose PurposeCondition
    | Condition_Story StoryCondition
    | Condition_Term_Months TermMonthsCondition
    | Condition_Term_Percent TermPercentCondition
    | Condition_Elapsed_Term_Months ElapsedTermMonthsCondition
    | Condition_Elapsed_Term_Percent ElapsedTermPercentCondition
    | Condition_Amount AmountCondition
    | Condition_Interest InterestCondition


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

        Condition_Purpose purposeCond ->
            renderPurposeCondition purposeCond

        Condition_Story storyCond ->
            renderStoryCondition storyCond

        Condition_Term_Months termMonthsCond ->
            renderTermMonthsCondition termMonthsCond

        Condition_Term_Percent termPercentCond ->
            renderTermPercentCondition termPercentCond

        Condition_Elapsed_Term_Months elapsedTermMonthsCond ->
            renderElapsedTermMonthsCondition elapsedTermMonthsCond

        Condition_Elapsed_Term_Percent elapsedTermPercentCond ->
            renderElapsedTermPercentCondition elapsedTermPercentCond

        Condition_Amount amountCond ->
            renderAmountCondition amountCond

        Condition_Interest interestCond ->
            renderInterestCondition interestCond


conditionsValidationErrors : String -> Conditions -> List String
conditionsValidationErrors errorPrefix =
    List.map (\e -> errorPrefix ++ e) << List.concat << List.map conditionValidationError << conditionsToList


conditionValidationError : Condition -> List String
conditionValidationError c =
    case c of
        Condition_Region regionCond ->
            Region.validationErrors regionCond

        Condition_Rating ratingCond ->
            Rating.validationErrors ratingCond

        Condition_Income incomeCond ->
            MainIncome.validationErrors incomeCond

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

        Condition_Amount amountCond ->
            Amount.validationErrors amountCond

        Condition_Interest interestCond ->
            Interest.validationErrors interestCond


conditionsToList : Conditions -> List Condition
conditionsToList { region, rating, income, purpose, story, termMonths, termPercent, elapsedTermMonths, elapsedTermPercent, amount, interest } =
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

        terM =
            fromMaybe Condition_Term_Months termMonths

        terP =
            fromMaybe Condition_Term_Percent termPercent

        elTermM =
            fromMaybe Condition_Elapsed_Term_Months elapsedTermMonths

        elTermP =
            fromMaybe Condition_Elapsed_Term_Percent elapsedTermPercent

        amo =
            fromMaybe Condition_Amount amount

        inte =
            fromMaybe Condition_Interest interest
    in
    List.concat [ reg, rat, inc, pur, sto, terM, terP, elTermM, elTermP, amo, inte ]


addCondition : Condition -> Conditions -> Conditions
addCondition c cs =
    case c of
        Condition_Region regionCond ->
            setRegionCondition regionCond cs

        Condition_Rating ratingCond ->
            setRatingCondition ratingCond cs

        Condition_Income incomeCond ->
            setIncomeCondition incomeCond cs

        Condition_Purpose purposeCond ->
            setPurposeCondition purposeCond cs

        Condition_Story storyCond ->
            setStoryCondition storyCond cs

        Condition_Term_Months termMonthsCond ->
            setTermMonthsCondition termMonthsCond cs

        Condition_Term_Percent termPercentCond ->
            setTermPercentCondition termPercentCond cs

        Condition_Elapsed_Term_Months elapsedTermMonthsCond ->
            setElapsedTermMonthsCondition elapsedTermMonthsCond cs

        Condition_Elapsed_Term_Percent elapsedTermPercentCond ->
            setElapsedTermPercentCondition elapsedTermPercentCond cs

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


removeTermMonthsCondition : Conditions -> Conditions
removeTermMonthsCondition cs =
    { cs | termMonths = Nothing }


removeTermPercentCondition : Conditions -> Conditions
removeTermPercentCondition cs =
    { cs | termPercent = Nothing }


removeElapsedTermMonthsCondition : Conditions -> Conditions
removeElapsedTermMonthsCondition cs =
    { cs | elapsedTermMonths = Nothing }


removeElapsedTermPercentCondition : Conditions -> Conditions
removeElapsedTermPercentCondition cs =
    { cs | elapsedTermPercent = Nothing }


removeMainIncomeCondition : Conditions -> Conditions
removeMainIncomeCondition cs =
    { cs | income = Nothing }


removeRatingCondition : Conditions -> Conditions
removeRatingCondition cs =
    { cs | rating = Nothing }


removeRegionCondition : Conditions -> Conditions
removeRegionCondition cs =
    { cs | region = Nothing }



-- JSON


encodeConditions : Conditions -> Value
encodeConditions { region, rating, income, purpose, story, termMonths, termPercent, elapsedTermMonths, elapsedTermPercent, interest, amount } =
    Encode.object
        [ ( "region", encodeMaybe Region.encodeCondition region )
        , ( "rating", encodeMaybe Rating.encodeCondition rating )
        , ( "income", encodeMaybe MainIncome.encodeCondition income )
        , ( "purpose", encodeMaybe Purpose.encodeCondition purpose )
        , ( "story", encodeMaybe Story.encodeCondition story )
        , ( "termMonths", encodeMaybe TermMonths.encodeCondition termMonths )
        , ( "termPercent", encodeMaybe TermPercent.encodeCondition termPercent )
        , ( "elapsedTermMonths", encodeMaybe ElapsedTermMonths.encodeCondition elapsedTermMonths )
        , ( "elapsedTermPercent", encodeMaybe ElapsedTermPercent.encodeCondition elapsedTermPercent )
        , ( "interest", encodeMaybe Interest.encodeCondition interest )
        , ( "amount", encodeMaybe Amount.encodeCondition amount )
        ]


encodeMaybe : (a -> Value) -> Maybe a -> Value
encodeMaybe enc =
    Maybe.map enc >> Maybe.withDefault Encode.null


conditionsDecoder : Decoder Conditions
conditionsDecoder =
    Decode.succeed Conditions
        |: (field "region" <| nullable Region.conditionDecoder)
        |: (field "rating" <| nullable Rating.conditionDecoder)
        |: (field "income" <| nullable MainIncome.conditionDecoder)
        |: (field "purpose" <| nullable Purpose.conditionDecoder)
        |: (field "story" <| nullable Story.conditionDecoder)
        |: (field "termMonths" <| nullable TermMonths.conditionDecoder)
        |: (field "termPercent" <| nullable TermPercent.conditionDecoder)
        |: (field "elapsedTermMonths" <| nullable ElapsedTermMonths.conditionDecoder)
        |: (field "elapsedTermPercent" <| nullable ElapsedTermPercent.conditionDecoder)
        |: (field "interest" <| nullable Interest.conditionDecoder)
        |: (field "amount" <| nullable Amount.conditionDecoder)
