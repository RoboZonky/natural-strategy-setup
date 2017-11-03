module View.Filter.Conditions exposing (..)

import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Data.Filter exposing (FilteredItem(..))
import Data.Filter.Conditions exposing (..)
import Data.Filter.Conditions.Amount as Amount exposing (Amount(..), AmountCondition(..), AmountMsg)
import Data.Filter.Conditions.Interest as Interest exposing (Interest(..), InterestCondition(..), InterestMsg)
import Data.Filter.Conditions.MainIncome as MainIncome exposing (MainIncome(..), MainIncomeCondition(..), MainIncomeMsg)
import Data.Filter.Conditions.Purpose as Purpose exposing (Purpose(..), PurposeCondition(..), PurposeMsg)
import Data.Filter.Conditions.Rating as Rating exposing (Rating(..), RatingCondition(..), RatingMsg)
import Data.Filter.Conditions.Region as Region exposing (Region(..), RegionCondition(..), RegionMsg)
import Data.Filter.Conditions.Story as Story exposing (Story(..), StoryCondition(..), StoryMsg)
import Data.Filter.Conditions.TermMonths as TermMonths exposing (TermMonths(..), TermMonthsCondition(..), TermMonthsMsg)
import Data.Filter.Conditions.TermPercent as TermPercent exposing (TermPercent(..), TermPercentCondition(..), TermPercentMsg)
import Html exposing (Html, div, text)


-- MODEL


type alias Model =
    Conditions



-- VIEW


conditionsForm : FilteredItem -> Conditions -> Html Msg
conditionsForm filteredItem conditions =
    let
        extraRows =
            case filteredItem of
                Loan ->
                    [ amountRow ]

                Participation ->
                    [ termPercentRow ]

                Participation_To_Sell ->
                    [ termPercentRow ]

                Loan_And_Participation ->
                    []

        termPercentRow =
            conditionRow conditions (termConditionLabel filteredItem "(v procentech)") (Condition_Term_Percent (TermPercentCondition (TermPercent.LessThan 0))) RemoveTermPercentContion

        amountRow =
            conditionRow conditions "Výše úvěru" (Condition_Amount (AmountCondition (Amount.LessThan 0))) RemoveAmountCondition
    in
    div [] <|
        List.concat
            [ [ conditionRow conditions "Rating" (Condition_Rating (RatingList [])) RemoveRatingCondition
              , conditionRow conditions "Úrok" (Condition_Interest (InterestCondition (Interest.LessThan 0))) RemoveInterestCondition
              , conditionRow conditions "Účel úvěru" (Condition_Purpose (PurposeList [])) RemovePurposeCondition
              , conditionRow conditions (termConditionLabel filteredItem "(v měsících)") (Condition_Term_Months (TermMonthsCondition (TermMonths.LessThan 0))) RemoveTermMonthsCondition
              ]

            -- put extra rows "in the middle" to have 'Délka úvěru (v měsících)' and 'Délka úvěru (v procentech)' next to each other"
            , extraRows
            , [ conditionRow conditions "Zdroj příjmů klienta" (Condition_Income (MainIncomeList [])) RemoveMainIncomeCondition
              , conditionRow conditions "Příběh" (Condition_Story (StoryCondition SHORT)) RemoveStoryCondition
              , conditionRow conditions "Kraj klienta" (Condition_Region (RegionList [])) RemoveRegionCondition
              ]
            ]


termConditionLabel : FilteredItem -> String -> String
termConditionLabel filteredItem unitStr =
    case filteredItem of
        Loan ->
            "Délka úvěru " ++ unitStr

        _ ->
            "Zbývající délka úvěru " ++ unitStr


conditionRow : Conditions -> String -> Condition -> Msg -> Html Msg
conditionRow conditions conditionName condition removeCondMsg =
    let
        onChk checked =
            if checked then
                AddCondition condition
            else
                removeCondMsg

        ( isSubformEnabled, subform ) =
            case condition of
                Condition_Amount _ ->
                    ( subformEnabled conditions.amount, showFormForNonemptyCondition AmountMsg Amount.amountForm conditions.amount )

                Condition_Income _ ->
                    ( subformEnabled conditions.income, showFormForNonemptyCondition MainIncomeMsg MainIncome.mainIncomeForm conditions.income )

                Condition_Interest _ ->
                    ( subformEnabled conditions.interest, showFormForNonemptyCondition InterestMsg Interest.interestForm conditions.interest )

                Condition_Purpose _ ->
                    ( subformEnabled conditions.purpose, showFormForNonemptyCondition PurposeMsg Purpose.purposeForm conditions.purpose )

                Condition_Term_Months _ ->
                    ( subformEnabled conditions.termMonths, showFormForNonemptyCondition TermMonthsMsg TermMonths.termMonthsForm conditions.termMonths )

                Condition_Term_Percent _ ->
                    ( subformEnabled conditions.termPercent, showFormForNonemptyCondition TermPercentMsg TermPercent.termPercentForm conditions.termPercent )

                Condition_Region _ ->
                    ( subformEnabled conditions.region, showFormForNonemptyCondition RegionMsg Region.regionForm conditions.region )

                Condition_Rating _ ->
                    ( subformEnabled conditions.rating, showFormForNonemptyCondition RatingMsg Rating.ratingForm conditions.rating )

                Condition_Story _ ->
                    ( subformEnabled conditions.story, showFormForNonemptyCondition StoryMsg Story.storyForm conditions.story )
    in
    Grid.row []
        [ Grid.col [ Col.xs3 ] [ Checkbox.checkbox [ Checkbox.checked isSubformEnabled, Checkbox.onCheck onChk ] conditionName ]
        , Grid.col [ Col.xs9 ] [ subform ]
        ]


subformEnabled : Maybe a -> Bool
subformEnabled mCondition =
    case mCondition of
        Nothing ->
            False

        _ ->
            True


showFormForNonemptyCondition : (condMsg -> Msg) -> (condition -> Html condMsg) -> Maybe condition -> Html Msg
showFormForNonemptyCondition condWrapper condForm =
    Maybe.withDefault (text "") << Maybe.map (Html.map condWrapper << condForm)



-- UPDATE


type Msg
    = InterestMsg InterestMsg
    | AmountMsg AmountMsg
    | StoryMsg StoryMsg
    | PurposeMsg PurposeMsg
    | TermMonthsMsg TermMonthsMsg
    | TermPercentMsg TermPercentMsg
    | MainIncomeMsg MainIncomeMsg
    | RatingMsg RatingMsg
    | RegionMsg RegionMsg
    | AddCondition Condition
    | RemoveInterestCondition
    | RemoveAmountCondition
    | RemoveStoryCondition
    | RemovePurposeCondition
    | RemoveTermMonthsCondition
    | RemoveTermPercentContion
    | RemoveMainIncomeCondition
    | RemoveRatingCondition
    | RemoveRegionCondition


update : Msg -> Model -> Model
update msg model =
    case msg of
        RatingMsg msg ->
            updateRating msg model

        InterestMsg msg ->
            updateInterest msg model

        PurposeMsg msg ->
            updatePurpose msg model

        TermMonthsMsg msg ->
            updateTermMonths msg model

        TermPercentMsg msg ->
            updateTermPercent msg model

        MainIncomeMsg msg ->
            updateMainIncome msg model

        StoryMsg msg ->
            updateStory msg model

        AmountMsg msg ->
            updateAmount msg model

        RegionMsg msg ->
            updateRegion msg model

        AddCondition c ->
            addCondition c model

        RemoveInterestCondition ->
            removeInterestCondition model

        RemoveAmountCondition ->
            removeAmountCondition model

        RemoveStoryCondition ->
            removeStoryCondition model

        RemovePurposeCondition ->
            removePurposeCondition model

        RemoveTermMonthsCondition ->
            removeTermMonthsCondition model

        RemoveTermPercentContion ->
            removeTermPercentCondition model

        RemoveMainIncomeCondition ->
            removeMainIncomeCondition model

        RemoveRatingCondition ->
            removeRatingCondition model

        RemoveRegionCondition ->
            removeRegionCondition model
