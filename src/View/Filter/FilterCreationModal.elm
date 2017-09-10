module View.Filter.FilterCreationModal exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Data.Filter as Filter exposing (Condition(..), Conditions, FilteredItem(..), MarketplaceFilter(..), renderMarketplaceFilter, setFilteredItem)
import Data.Filter.Condition.Amount as Amount exposing (Amount(..), AmountCondition(..), AmountMsg)
import Data.Filter.Condition.Interest as Interest exposing (Interest(..), InterestCondition(..), InterestMsg)
import Data.Filter.Condition.LoanPurpose as LoanPurpose exposing (LoanPurposeCondition(..), PurposeMsg)
import Data.Filter.Condition.LoanTerm as LoanTerm exposing (LoanTermMsg, TermCondition(..))
import Data.Filter.Condition.MainIncome as MainIncome exposing (MainIncome(..), MainIncomeCondition(..), MainIncomeMsg)
import Data.Filter.Condition.Rating as Rating exposing (RatingCondition(..), RatingMsg)
import Data.Filter.Condition.Region as Region exposing (Region(..), RegionCondition(..), RegionMsg)
import Data.Filter.Condition.Story as Story exposing (Story(..), StoryCondition(..), StoryMsg)
import Html exposing (Html, div, hr, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onSubmit)
import Types exposing (..)


type alias State =
    { editedFilter : MarketplaceFilter
    , openCloseState : Modal.State
    }


initialState : State
initialState =
    { editedFilter = Filter.emptyFilter
    , openCloseState = Modal.hiddenState
    }


update : ModalMsg -> State -> State
update msg state =
    case msg of
        FilteredItemChange item ->
            { state | editedFilter = removeAmountConditionIfNotFilteringLoan item <| setFilteredItem item state.editedFilter }

        OpenOrClose st ->
            { state | openCloseState = st }

        InterestMsg msg ->
            { state | editedFilter = updateInterest msg state.editedFilter }

        AmountMsg msg ->
            { state | editedFilter = updateAmount msg state.editedFilter }

        StoryMsg msg ->
            { state | editedFilter = updateStory msg state.editedFilter }

        PurposeMsg msg ->
            { state | editedFilter = updatePurpose msg state.editedFilter }

        LoanTermMsg msg ->
            { state | editedFilter = updateLoanTerm msg state.editedFilter }

        MainIncomeMsg msg ->
            { state | editedFilter = updateMainIncome msg state.editedFilter }

        RatingMsg msg ->
            { state | editedFilter = updateRating msg state.editedFilter }

        RegionMsg msg ->
            { state | editedFilter = updateRegion msg state.editedFilter }

        AddCondition c ->
            { state | editedFilter = Filter.addPositiveCondition c state.editedFilter }

        RemoveInterestCondition ->
            { state | editedFilter = Filter.removePositiveInterestCondition state.editedFilter }

        RemoveAmountCondition ->
            { state | editedFilter = Filter.removePositiveAmountCondition state.editedFilter }

        RemoveStoryCondition ->
            { state | editedFilter = Filter.removePositiveStoryCondition state.editedFilter }

        RemovePurposeCondition ->
            { state | editedFilter = Filter.removePositivePurposeCondition state.editedFilter }

        RemoveTermCondition ->
            { state | editedFilter = Filter.removePositiveTermCondition state.editedFilter }

        RemoveMainIncomeCondition ->
            { state | editedFilter = Filter.removePositiveIncomeCondition state.editedFilter }

        RemoveRatingCondition ->
            { state | editedFilter = Filter.removePositiveRatingCondition state.editedFilter }

        RemoveRegionCondition ->
            { state | editedFilter = Filter.removePositiveRegionCondition state.editedFilter }

        ModalNoOp ->
            state


view : State -> Html ModalMsg
view { editedFilter, openCloseState } =
    Modal.config OpenOrClose
        |> Modal.large
        |> Modal.h5 [] [ text "Vytvořit filtr" ]
        |> Modal.body []
            [ modalBody editedFilter
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.primary
                , Button.attrs [ onClick <| OpenOrClose Modal.hiddenState ]
                ]
                [ text "Přidat" ]
            , Button.button
                [ Button.danger
                , Button.attrs [ onClick <| OpenOrClose Modal.hiddenState ]
                ]
                [ text "Zrušit" ]
            ]
        |> Modal.view openCloseState


modalBody : MarketplaceFilter -> Html ModalMsg
modalBody (MarketplaceFilter state) =
    Grid.containerFluid []
        [ Grid.row []
            [ Grid.col
                [ Col.xs12 ]
                [ whatToFilterForm
                , positiveConditionsForm state.whatToFilter state.ignoreWhen
                , hr [] []
                , text <| renderMarketplaceFilter (MarketplaceFilter state)
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
                    [ conditionRow "Výše úvěru" (AddCondition (Condition_Amount (AmountCondition (Amount.LessThan 0)))) RemoveAmountCondition (amountForm conditions.amount) ]

                _ ->
                    []
    in
    div [] <|
        [ conditionRow "Rating" (AddCondition (Condition_Rating (RatingList []))) RemoveRatingCondition (ratingForm conditions.rating)
        , conditionRow "Úrok" (AddCondition (Condition_Interest (InterestCondition (Interest.LessThan 0)))) RemoveInterestCondition (interestForm conditions.interest)
        , conditionRow "Účel úvěru" (AddCondition (Condition_Purpose (LoanPurposeList []))) RemovePurposeCondition (purposeForm conditions.purpose)
        , conditionRow "Délka úvěru" (AddCondition (Condition_Term (TermCondition (LoanTerm.LessThan 0)))) RemoveTermCondition (termForm conditions.term)
        , conditionRow "Zdroj příjmů klienta" (AddCondition (Condition_Income (MainIncomeList []))) RemoveMainIncomeCondition (mainIncomeForm conditions.income)
        , conditionRow "Příběh" (AddCondition (Condition_Story (StoryCondition SHORT))) RemoveStoryCondition (storyForm conditions.story)
        , conditionRow "Kraj klienta" (AddCondition (Condition_Region (RegionList []))) RemoveRegionCondition (regionForm conditions.region)
        ]
            ++ amountRowOnlyEnabledForLoans


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


updateInterest : InterestMsg -> MarketplaceFilter -> MarketplaceFilter
updateInterest msg (MarketplaceFilter f) =
    let
        { ignoreWhen } =
            f

        newIgnoreWhen =
            { ignoreWhen | interest = Maybe.map (Interest.update msg) ignoreWhen.interest }
    in
    MarketplaceFilter { f | ignoreWhen = newIgnoreWhen }


updateAmount : AmountMsg -> MarketplaceFilter -> MarketplaceFilter
updateAmount msg (MarketplaceFilter f) =
    let
        { ignoreWhen } =
            f

        newIgnoreWhen =
            { ignoreWhen | amount = Maybe.map (Amount.update msg) ignoreWhen.amount }
    in
    MarketplaceFilter { f | ignoreWhen = newIgnoreWhen }


updateStory : StoryMsg -> MarketplaceFilter -> MarketplaceFilter
updateStory msg (MarketplaceFilter f) =
    let
        { ignoreWhen } =
            f

        newIgnoreWhen =
            { ignoreWhen | story = Maybe.map (Story.update msg) ignoreWhen.story }
    in
    MarketplaceFilter { f | ignoreWhen = newIgnoreWhen }


updatePurpose : PurposeMsg -> MarketplaceFilter -> MarketplaceFilter
updatePurpose msg (MarketplaceFilter f) =
    let
        { ignoreWhen } =
            f

        newIgnoreWhen =
            { ignoreWhen | purpose = Maybe.map (LoanPurpose.update msg) ignoreWhen.purpose }
    in
    MarketplaceFilter { f | ignoreWhen = newIgnoreWhen }


updateLoanTerm : LoanTermMsg -> MarketplaceFilter -> MarketplaceFilter
updateLoanTerm msg (MarketplaceFilter f) =
    let
        { ignoreWhen } =
            f

        newIgnoreWhen =
            { ignoreWhen | term = Maybe.map (LoanTerm.update msg) ignoreWhen.term }
    in
    MarketplaceFilter { f | ignoreWhen = newIgnoreWhen }


updateMainIncome : MainIncomeMsg -> MarketplaceFilter -> MarketplaceFilter
updateMainIncome msg (MarketplaceFilter f) =
    let
        { ignoreWhen } =
            f

        newIgnoreWhen =
            { ignoreWhen | income = Maybe.map (MainIncome.update msg) ignoreWhen.income }
    in
    MarketplaceFilter { f | ignoreWhen = newIgnoreWhen }


updateRating : RatingMsg -> MarketplaceFilter -> MarketplaceFilter
updateRating msg (MarketplaceFilter f) =
    let
        { ignoreWhen } =
            f

        newIgnoreWhen =
            { ignoreWhen | rating = Maybe.map (Rating.update msg) ignoreWhen.rating }
    in
    MarketplaceFilter { f | ignoreWhen = newIgnoreWhen }


updateRegion : RegionMsg -> MarketplaceFilter -> MarketplaceFilter
updateRegion msg (MarketplaceFilter f) =
    let
        { ignoreWhen } =
            f

        newIgnoreWhen =
            { ignoreWhen | region = Maybe.map (Region.update msg) ignoreWhen.region }
    in
    MarketplaceFilter { f | ignoreWhen = newIgnoreWhen }


conditionRow : String -> ModalMsg -> ModalMsg -> Html ModalMsg -> Html ModalMsg
conditionRow conditionName addCondMsg removeCondMsg subform =
    let
        onChk checked =
            if checked then
                addCondMsg
            else
                removeCondMsg
    in
    Grid.row []
        [ Grid.col [ Col.xs3 ] [ Checkbox.checkbox [ Checkbox.onCheck onChk ] conditionName ]
        , Grid.col [ Col.xs9 ] [ subform ]
        ]


removeAmountConditionIfNotFilteringLoan : FilteredItem -> MarketplaceFilter -> MarketplaceFilter
removeAmountConditionIfNotFilteringLoan filteredItem mf =
    if filteredItem /= Loan then
        Filter.removePositiveAmountCondition mf
    else
        mf
