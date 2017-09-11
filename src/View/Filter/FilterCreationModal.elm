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
import Html exposing (Html, div, hr, li, text, ul)
import Html.Attributes exposing (class, style, value)
import Html.Events exposing (onClick, onSubmit)
import Types exposing (..)
import Util exposing ((=>))


type alias State =
    { editedFilter : MarketplaceFilter
    , openCloseState : Modal.State
    }


initialState : State
initialState =
    { editedFilter = Filter.emptyFilter
    , openCloseState = Modal.hiddenState
    }


update : ModalMsg -> State -> ( State, Maybe MarketplaceFilter )
update msg state =
    case msg of
        FilteredItemChange item ->
            { state | editedFilter = removeAmountConditionIfNotFilteringLoan item <| setFilteredItem item state.editedFilter } => Nothing

        ModalStateMsg st ->
            { state | editedFilter = Filter.emptyFilter, openCloseState = st } => Nothing

        InterestMsg msg ->
            { state | editedFilter = updateInterest msg state.editedFilter } => Nothing

        AmountMsg msg ->
            { state | editedFilter = updateAmount msg state.editedFilter } => Nothing

        StoryMsg msg ->
            { state | editedFilter = updateStory msg state.editedFilter } => Nothing

        PurposeMsg msg ->
            { state | editedFilter = updatePurpose msg state.editedFilter } => Nothing

        LoanTermMsg msg ->
            { state | editedFilter = updateLoanTerm msg state.editedFilter } => Nothing

        MainIncomeMsg msg ->
            { state | editedFilter = updateMainIncome msg state.editedFilter } => Nothing

        RatingMsg msg ->
            { state | editedFilter = updateRating msg state.editedFilter } => Nothing

        RegionMsg msg ->
            { state | editedFilter = updateRegion msg state.editedFilter } => Nothing

        AddCondition c ->
            { state | editedFilter = Filter.addPositiveCondition c state.editedFilter } => Nothing

        RemoveInterestCondition ->
            { state | editedFilter = Filter.removePositiveInterestCondition state.editedFilter } => Nothing

        RemoveAmountCondition ->
            { state | editedFilter = Filter.removePositiveAmountCondition state.editedFilter } => Nothing

        RemoveStoryCondition ->
            { state | editedFilter = Filter.removePositiveStoryCondition state.editedFilter } => Nothing

        RemovePurposeCondition ->
            { state | editedFilter = Filter.removePositivePurposeCondition state.editedFilter } => Nothing

        RemoveTermCondition ->
            { state | editedFilter = Filter.removePositiveTermCondition state.editedFilter } => Nothing

        RemoveMainIncomeCondition ->
            { state | editedFilter = Filter.removePositiveIncomeCondition state.editedFilter } => Nothing

        RemoveRatingCondition ->
            { state | editedFilter = Filter.removePositiveRatingCondition state.editedFilter } => Nothing

        RemoveRegionCondition ->
            { state | editedFilter = Filter.removePositiveRegionCondition state.editedFilter } => Nothing

        SaveFilter ->
            { state | editedFilter = Filter.emptyFilter, openCloseState = Modal.hiddenState } => Just state.editedFilter

        ModalNoOp ->
            state => Nothing


view : State -> Html ModalMsg
view { editedFilter, openCloseState } =
    Modal.config ModalStateMsg
        |> Modal.large
        |> Modal.h5 [] [ text "Vytvořit filtr" ]
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
            Filter.validationErrors mf

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


updateInterest : InterestMsg -> MarketplaceFilter -> MarketplaceFilter
updateInterest msg =
    let
        interestUpdater conditions =
            { conditions | interest = Maybe.map (Interest.update msg) conditions.interest }
    in
    updatePositiveCondition interestUpdater


updateAmount : AmountMsg -> MarketplaceFilter -> MarketplaceFilter
updateAmount msg =
    let
        amountUpdater conditions =
            { conditions | amount = Maybe.map (Amount.update msg) conditions.amount }
    in
    updatePositiveCondition amountUpdater


updateStory : StoryMsg -> MarketplaceFilter -> MarketplaceFilter
updateStory msg =
    let
        storyUpdater conditions =
            { conditions | story = Maybe.map (Story.update msg) conditions.story }
    in
    updatePositiveCondition storyUpdater


updatePurpose : PurposeMsg -> MarketplaceFilter -> MarketplaceFilter
updatePurpose msg =
    let
        purposeUpdater conditions =
            { conditions | purpose = Maybe.map (LoanPurpose.update msg) conditions.purpose }
    in
    updatePositiveCondition purposeUpdater


updateLoanTerm : LoanTermMsg -> MarketplaceFilter -> MarketplaceFilter
updateLoanTerm msg =
    let
        loanTermUpdater conditions =
            { conditions | term = Maybe.map (LoanTerm.update msg) conditions.term }
    in
    updatePositiveCondition loanTermUpdater


updateMainIncome : MainIncomeMsg -> MarketplaceFilter -> MarketplaceFilter
updateMainIncome msg =
    let
        mainIncomeUpdater conditions =
            { conditions | income = Maybe.map (MainIncome.update msg) conditions.income }
    in
    updatePositiveCondition mainIncomeUpdater


updateRating : RatingMsg -> MarketplaceFilter -> MarketplaceFilter
updateRating msg =
    let
        regionUpdater conditions =
            { conditions | rating = Maybe.map (Rating.update msg) conditions.rating }
    in
    updatePositiveCondition regionUpdater


updateRegion : RegionMsg -> MarketplaceFilter -> MarketplaceFilter
updateRegion msg =
    let
        regionUpdater conditions =
            { conditions | region = Maybe.map (Region.update msg) conditions.region }
    in
    updatePositiveCondition regionUpdater


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


removeAmountConditionIfNotFilteringLoan : FilteredItem -> MarketplaceFilter -> MarketplaceFilter
removeAmountConditionIfNotFilteringLoan filteredItem mf =
    if filteredItem /= Loan then
        Filter.removePositiveAmountCondition mf
    else
        mf
