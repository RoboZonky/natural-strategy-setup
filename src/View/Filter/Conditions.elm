module View.Filter.Conditions exposing (Model, Msg, form, update)

import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Data.Filter exposing (FilteredItem(..))
import Data.Filter.Conditions exposing (..)
import Data.Filter.Conditions.Amount as Amount exposing (Amount(..), AmountCondition(..), AmountMsg)
import Data.Filter.Conditions.ElapsedTermMonths as ElapsedTermMonths exposing (ElapsedTermMonths(..), ElapsedTermMonthsCondition(..), ElapsedTermMonthsMsg)
import Data.Filter.Conditions.ElapsedTermPercent as ElapsedTermPercent exposing (ElapsedTermPercent(..), ElapsedTermPercentCondition(..), ElapsedTermPercentMsg)
import Data.Filter.Conditions.Interest as Interest exposing (Interest(..), InterestCondition(..), InterestMsg)
import Data.Filter.Conditions.MainIncome as MainIncome exposing (MainIncome(..), MainIncomeCondition(..), MainIncomeMsg)
import Data.Filter.Conditions.Purpose as Purpose exposing (Purpose(..), PurposeCondition(..), PurposeMsg)
import Data.Filter.Conditions.Rating as Rating exposing (Rating(..), RatingCondition(..), RatingMsg)
import Data.Filter.Conditions.Region as Region exposing (Region(..), RegionCondition(..), RegionMsg)
import Data.Filter.Conditions.Story as Story exposing (Story(..), StoryCondition(..), StoryMsg)
import Data.Filter.Conditions.TermMonths as TermMonths exposing (TermMonths(..), TermMonthsCondition(..), TermMonthsMsg)
import Data.Filter.Conditions.TermPercent as TermPercent exposing (TermPercent(..), TermPercentCondition(..), TermPercentMsg)
import Html exposing (Html, div, text)
import Html.Attributes exposing (classList, style)


-- MODEL


type alias Model =
    Conditions



-- VIEW


form : FilteredItem -> Conditions -> Html Msg
form filteredItem conditions =
    let
        extraRows =
            case filteredItem of
                Loan ->
                    [ amountRow ]

                Participation ->
                    [ termPercentRow, elapsedTermMonthsRow, elapsedTermPercentRow ]

                Participation_To_Sell ->
                    [ termPercentRow, elapsedTermMonthsRow, elapsedTermPercentRow ]

                Loan_And_Participation ->
                    []

        amountRow =
            conditionRow conditions "Výše úvěru" (Condition_Amount Amount.defaultCondition) RemoveAmountCondition

        termPercentRow =
            conditionRow conditions (termConditionLabel filteredItem "(v %)") (Condition_Term_Percent TermPercent.defaultCondition) RemoveTermPercentContion

        elapsedTermMonthsRow =
            conditionRow conditions "Uhrazeno splátek (v měsících)" (Condition_Elapsed_Term_Months ElapsedTermMonths.defaultCondition) RemoveElapsedTermMonthsCondition

        elapsedTermPercentRow =
            conditionRow conditions "Uhrazeno splátek (v %)" (Condition_Elapsed_Term_Percent ElapsedTermPercent.defaultCondition) RemoveElapsedTermPercentCondition
    in
    div
        [ style
            [ ( "overflow-x", "hidden" )
            , ( "overflow-y", "auto" {- scrollbar so that modal footer doesn't disappear below the bottom of viewport, when too many conditions enabled -} )
            , ( "max-height", "60vh" {- 60% viewport height -} )
            ]
        ]
    <|
        [ conditionRow conditions "Rating" (Condition_Rating Rating.defaultCondition) RemoveRatingCondition
        , conditionRow conditions "Úrok" (Condition_Interest Interest.defaultCondition) RemoveInterestCondition
        , conditionRow conditions "Účel úvěru" (Condition_Purpose Purpose.defaultCondition) RemovePurposeCondition
        , conditionRow conditions "Zdroj příjmů klienta" (Condition_Income MainIncome.defaultCondition) RemoveMainIncomeCondition
        , conditionRow conditions "Příběh" (Condition_Story Story.defaultCondition) RemoveStoryCondition
        , conditionRow conditions "Kraj klienta" (Condition_Region Region.defaultCondition) RemoveRegionCondition
        , conditionRow conditions (termConditionLabel filteredItem "(v měsících)") (Condition_Term_Months TermMonths.defaultCondition) RemoveTermMonthsCondition
        ]
            ++ extraRows


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
                    ( subformEnabled conditions.amount, showFormForNonemptyCondition AmountMsg Amount.form conditions.amount )

                Condition_Income _ ->
                    ( subformEnabled conditions.income, showFormForNonemptyCondition MainIncomeMsg MainIncome.form conditions.income )

                Condition_Interest _ ->
                    ( subformEnabled conditions.interest, showFormForNonemptyCondition InterestMsg Interest.form conditions.interest )

                Condition_Purpose _ ->
                    ( subformEnabled conditions.purpose, showFormForNonemptyCondition PurposeMsg Purpose.form conditions.purpose )

                Condition_Term_Months _ ->
                    ( subformEnabled conditions.termMonths, showFormForNonemptyCondition TermMonthsMsg TermMonths.form conditions.termMonths )

                Condition_Term_Percent _ ->
                    ( subformEnabled conditions.termPercent, showFormForNonemptyCondition TermPercentMsg TermPercent.form conditions.termPercent )

                Condition_Elapsed_Term_Months _ ->
                    ( subformEnabled conditions.elapsedTermMonths, showFormForNonemptyCondition ElapsedTermMonthsMsg ElapsedTermMonths.form conditions.elapsedTermMonths )

                Condition_Elapsed_Term_Percent _ ->
                    ( subformEnabled conditions.elapsedTermPercent, showFormForNonemptyCondition ElapsedTermPercentMsg ElapsedTermPercent.form conditions.elapsedTermPercent )

                Condition_Region _ ->
                    ( subformEnabled conditions.region, showFormForNonemptyCondition RegionMsg Region.form conditions.region )

                Condition_Rating _ ->
                    ( subformEnabled conditions.rating, showFormForNonemptyCondition RatingMsg Rating.form conditions.rating )

                Condition_Story _ ->
                    ( subformEnabled conditions.story, showFormForNonemptyCondition StoryMsg Story.form conditions.story )
    in
    Fieldset.config
        |> Fieldset.asGroup
        |> Fieldset.attrs [ classList [ ( "condition-row", True ), ( "active", isSubformEnabled ) ] ]
        |> Fieldset.children
            [ Grid.row []
                [ Grid.col [ Col.xs5 ] [ Checkbox.checkbox [ Checkbox.checked isSubformEnabled, Checkbox.onCheck onChk ] conditionName ]
                , Grid.col [ Col.xs7 ] [ subform ]
                ]
            ]
        |> Fieldset.view


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
    | ElapsedTermMonthsMsg ElapsedTermMonthsMsg
    | ElapsedTermPercentMsg ElapsedTermPercentMsg
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
    | RemoveElapsedTermMonthsCondition
    | RemoveElapsedTermPercentCondition
    | RemoveMainIncomeCondition
    | RemoveRatingCondition
    | RemoveRegionCondition


update : Msg -> Model -> Model
update msg model =
    case msg of
        RatingMsg rmsg ->
            updateRating rmsg model

        InterestMsg imsg ->
            updateInterest imsg model

        PurposeMsg pmsg ->
            updatePurpose pmsg model

        TermMonthsMsg tmmsg ->
            updateTermMonths tmmsg model

        TermPercentMsg tpmsg ->
            updateTermPercent tpmsg model

        ElapsedTermMonthsMsg emsg ->
            updateElapsedTermMonths emsg model

        ElapsedTermPercentMsg emsg ->
            updateElapsedTermPercent emsg model

        MainIncomeMsg mimsg ->
            updateMainIncome mimsg model

        StoryMsg smsg ->
            updateStory smsg model

        AmountMsg amsg ->
            updateAmount amsg model

        RegionMsg rmsg ->
            updateRegion rmsg model

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

        RemoveElapsedTermMonthsCondition ->
            removeElapsedTermMonthsCondition model

        RemoveElapsedTermPercentCondition ->
            removeElapsedTermPercentCondition model

        RemoveMainIncomeCondition ->
            removeMainIncomeCondition model

        RemoveRatingCondition ->
            removeRatingCondition model

        RemoveRegionCondition ->
            removeRegionCondition model
