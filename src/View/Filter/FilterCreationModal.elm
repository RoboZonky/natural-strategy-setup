module View.Filter.FilterCreationModal exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Data.Filter as Filter exposing (Condition(..), Conditions, FilteredItem(..), MarketplaceFilter(..), renderMarketplaceFilter, setFilteredItem)
import Data.Filter.Condition.Amount as Amount exposing (AmountCondition(..))
import Data.Filter.Condition.Interest as Interest exposing (InterestCondition(..))
import Data.Filter.Condition.LoanPurpose as LoanPurpose exposing (LoanPurposeCondition(..))
import Data.Filter.Condition.LoanTerm as LoanTerm exposing (TermCondition(..))
import Data.Filter.Condition.MainIncome as MainIncome exposing (MainIncomeCondition(..))
import Data.Filter.Condition.Rating as Rating exposing (RatingCondition(..))
import Data.Filter.Condition.Region as Region exposing (RegionCondition(..))
import Data.Filter.Condition.Story as Story exposing (Story(SHORT), StoryCondition(..))
import Data.Tooltip as Tooltip
import Html exposing (Html, div, hr, li, text, ul)
import Html.Attributes exposing (class, style, value)
import Html.Events exposing (onClick, onSubmit)
import Types exposing (..)
import View.Tooltip as Tooltip


type alias Model =
    { editedFilter : MarketplaceFilter
    , withException : Bool
    , openCloseState : Modal.State
    }


initialState : Model
initialState =
    { editedFilter = Filter.emptyFilter
    , withException = False
    , openCloseState = Modal.hiddenState
    }


update : ModalMsg -> Model -> ( Model, Maybe MarketplaceFilter )
update msg state =
    case msg of
        SaveFilter ->
            ( updateHelp msg state, Just state.editedFilter )

        _ ->
            ( updateHelp msg state, Nothing )


{-| Inner modal messages that don't produce Filter to be added to the main app's model
-}
updateHelp : ModalMsg -> Model -> Model
updateHelp msg state =
    case msg of
        FilteredItemChange item ->
            { state | editedFilter = updatePositiveCondition (removeAmountConditionIfNotFilteringLoan item) <| setFilteredItem item state.editedFilter }

        ModalStateMsg st ->
            { state | editedFilter = Filter.emptyFilter, openCloseState = st }

        RatingMsg msg ->
            { state | editedFilter = updatePositiveCondition (Filter.updateRating msg) state.editedFilter }

        InterestMsg msg ->
            { state | editedFilter = updatePositiveCondition (Filter.updateInterest msg) state.editedFilter }

        PurposeMsg msg ->
            { state | editedFilter = updatePositiveCondition (Filter.updatePurpose msg) state.editedFilter }

        LoanTermMsg msg ->
            { state | editedFilter = updatePositiveCondition (Filter.updateLoanTerm msg) state.editedFilter }

        MainIncomeMsg msg ->
            { state | editedFilter = updatePositiveCondition (Filter.updateMainIncome msg) state.editedFilter }

        StoryMsg msg ->
            { state | editedFilter = updatePositiveCondition (Filter.updateStory msg) state.editedFilter }

        AmountMsg msg ->
            { state | editedFilter = updatePositiveCondition (Filter.updateAmount msg) state.editedFilter }

        RegionMsg msg ->
            { state | editedFilter = updatePositiveCondition (Filter.updateRegion msg) state.editedFilter }

        AddCondition c ->
            { state | editedFilter = Filter.addPositiveCondition c state.editedFilter }

        RemoveInterestCondition ->
            { state | editedFilter = updatePositiveCondition Filter.removeInterestCondition state.editedFilter }

        RemoveAmountCondition ->
            { state | editedFilter = updatePositiveCondition Filter.removeAmountCondition state.editedFilter }

        RemoveStoryCondition ->
            { state | editedFilter = updatePositiveCondition Filter.removeStoryCondition state.editedFilter }

        RemovePurposeCondition ->
            { state | editedFilter = updatePositiveCondition Filter.removePurposeCondition state.editedFilter }

        RemoveTermCondition ->
            { state | editedFilter = updatePositiveCondition Filter.removeLoanTermCondition state.editedFilter }

        RemoveMainIncomeCondition ->
            { state | editedFilter = updatePositiveCondition Filter.removeMainIncomeCondition state.editedFilter }

        RemoveRatingCondition ->
            { state | editedFilter = updatePositiveCondition Filter.removeRatingCondition state.editedFilter }

        RemoveRegionCondition ->
            { state | editedFilter = updatePositiveCondition Filter.removeRegionCondition state.editedFilter }

        ModalTooltipMsg tipId tooltipState ->
            {- This case is handled at the level of Main's update -}
            state

        SaveFilter ->
            { state | editedFilter = Filter.emptyFilter, openCloseState = Modal.hiddenState }

        ModalNoOp ->
            state


view : Model -> Tooltip.States -> Html ModalMsg
view { editedFilter, openCloseState } tooltipStates =
    Modal.config ModalStateMsg
        |> Modal.large
        |> Modal.h5 []
            [ text "Vytvořit filtr"
            , Tooltip.popoverTipForModal Tooltip.filterCreationTip tooltipStates
            ]
        |> Modal.body []
            [ modalBody editedFilter
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.primary
                , Button.disabled (not <| Filter.isValid editedFilter)
                , Button.attrs [ onClick SaveFilter ]
                ]
                [ text "Přidat" ]
            , Button.button
                [ Button.danger
                , Button.attrs [ onClick <| ModalStateMsg Modal.hiddenState ]
                ]
                [ text "Zrušit" ]
            ]
        |> Modal.view openCloseState


modalBody : MarketplaceFilter -> Html ModalMsg
modalBody ((MarketplaceFilter state) as mf) =
    let
        validationErrors =
            Filter.marketplaceFilterValidationErrors mf

        previewOrValidationErrors =
            if List.isEmpty validationErrors then
                text <| renderMarketplaceFilter mf
            else
                ul [ style [ ( "color", "red" ) ] ] <|
                    List.map (\e -> li [] [ text e ]) validationErrors
    in
    Grid.containerFluid []
        [ Grid.row []
            [ Grid.col
                [ Col.xs12 ]
                [ whatToFilterForm
                , positiveConditionsForm state.whatToFilter state.ignoreWhen
                , hr [] []
                , previewOrValidationErrors
                ]
            ]
        ]


whatToFilterForm : Html ModalMsg
whatToFilterForm =
    Form.formInline [ onSubmit ModalNoOp ]
        [ text "Ignorovat ", whatToFilterSelect, text ", kde:" ]


whatToFilterSelect : Html ModalMsg
whatToFilterSelect =
    let
        optionList =
            List.map
                (\itm ->
                    Select.item
                        [ value (toString itm) ]
                        [ text (Filter.renderFilteredItem itm) ]
                )
                [ Loan, Participation, Loan_And_Participation ]
    in
    Select.select
        [ Select.small
        , Select.onChange (FilteredItemChange << Filter.filtereedItemFromString)
        , Select.attrs [ class "mx-1" ]
        ]
        optionList


positiveConditionsForm : FilteredItem -> Conditions -> Html ModalMsg
positiveConditionsForm filteredItem conditions =
    let
        amountRowOnlyEnabledForLoans =
            case filteredItem of
                Loan ->
                    [ conditionRow "Výše úvěru" (subformEnabled conditions.amount) (Condition_Amount (AmountCondition (Amount.LessThan 0))) RemoveAmountCondition (amountForm conditions.amount) ]

                _ ->
                    []
    in
    div [] <|
        [ conditionRow "Rating" (subformEnabled conditions.rating) (Condition_Rating (RatingList [])) RemoveRatingCondition (ratingForm conditions.rating)
        , conditionRow "Úrok" (subformEnabled conditions.interest) (Condition_Interest (InterestCondition (Interest.LessThan 0))) RemoveInterestCondition (interestForm conditions.interest)
        , conditionRow "Účel úvěru" (subformEnabled conditions.purpose) (Condition_Purpose (LoanPurposeList [])) RemovePurposeCondition (purposeForm conditions.purpose)
        , conditionRow "Délka úvěru" (subformEnabled conditions.term) (Condition_Term (TermCondition (LoanTerm.LessThan 0))) RemoveTermCondition (termForm conditions.term)
        , conditionRow "Zdroj příjmů klienta" (subformEnabled conditions.income) (Condition_Income (MainIncomeList [])) RemoveMainIncomeCondition (mainIncomeForm conditions.income)
        , conditionRow "Příběh" (subformEnabled conditions.story) (Condition_Story (StoryCondition SHORT)) RemoveStoryCondition (storyForm conditions.story)
        , conditionRow "Kraj klienta" (subformEnabled conditions.region) (Condition_Region (RegionList [])) RemoveRegionCondition (regionForm conditions.region)
        ]
            ++ amountRowOnlyEnabledForLoans


subformEnabled : Maybe a -> Bool
subformEnabled mCondition =
    case mCondition of
        Nothing ->
            False

        _ ->
            True


ratingForm : Maybe RatingCondition -> Html ModalMsg
ratingForm =
    showFormForNonemptyCondition RatingMsg Rating.ratingForm


amountForm : Maybe AmountCondition -> Html ModalMsg
amountForm =
    showFormForNonemptyCondition AmountMsg Amount.amountForm


interestForm : Maybe InterestCondition -> Html ModalMsg
interestForm =
    showFormForNonemptyCondition InterestMsg Interest.interestForm


purposeForm : Maybe LoanPurposeCondition -> Html ModalMsg
purposeForm =
    showFormForNonemptyCondition PurposeMsg LoanPurpose.loanPurposeForm


termForm : Maybe TermCondition -> Html ModalMsg
termForm =
    showFormForNonemptyCondition LoanTermMsg LoanTerm.loanTermForm


mainIncomeForm : Maybe MainIncomeCondition -> Html ModalMsg
mainIncomeForm =
    showFormForNonemptyCondition MainIncomeMsg MainIncome.mainIncomeForm


storyForm : Maybe StoryCondition -> Html ModalMsg
storyForm =
    showFormForNonemptyCondition StoryMsg Story.storyForm


regionForm : Maybe RegionCondition -> Html ModalMsg
regionForm =
    showFormForNonemptyCondition RegionMsg Region.regionForm


showFormForNonemptyCondition : (condMsg -> ModalMsg) -> (condition -> Html condMsg) -> Maybe condition -> Html ModalMsg
showFormForNonemptyCondition condWrapper condForm =
    Maybe.withDefault (text "") << Maybe.map (Html.map condWrapper << condForm)


updatePositiveCondition : (Conditions -> Conditions) -> MarketplaceFilter -> MarketplaceFilter
updatePositiveCondition conditionsUpdater (MarketplaceFilter f) =
    MarketplaceFilter { f | ignoreWhen = conditionsUpdater f.ignoreWhen }


conditionRow : String -> Bool -> Condition -> ModalMsg -> Html ModalMsg -> Html ModalMsg
conditionRow conditionName isSubformEnabled condition removeCondMsg subform =
    let
        onChk checked =
            if checked then
                AddCondition condition
            else
                removeCondMsg
    in
    Grid.row []
        [ Grid.col [ Col.xs3 ] [ Checkbox.checkbox [ Checkbox.checked isSubformEnabled, Checkbox.onCheck onChk ] conditionName ]
        , Grid.col [ Col.xs9 ] [ subform ]
        ]


removeAmountConditionIfNotFilteringLoan : FilteredItem -> Conditions -> Conditions
removeAmountConditionIfNotFilteringLoan filteredItem cs =
    if filteredItem /= Loan then
        Filter.removeAmountCondition cs
    else
        cs
