module View.Filter.FilterCreationModal exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Data.Filter as Filter exposing (Condition(..), Conditions, FilteredItem(..), MarketplaceFilter(..), renderMarketplaceFilter, setFilteredItem)
import Data.Filter.Condition.Amount as Amount exposing (Amount(..), AmountCondition(..), AmountMsg(..))
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
            { state | editedFilter = setFilteredItem item state.editedFilter }

        OpenOrClose st ->
            { state | openCloseState = st }

        AmountMsg msg ->
            { state | editedFilter = updateAmount msg state.editedFilter }

        AddCondition c ->
            { state | editedFilter = Filter.addPositiveCondition c state.editedFilter }

        RemoveAmountCondition ->
            { state | editedFilter = Filter.removePositiveAmountCondition state.editedFilter }

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
                    [ conditionRow "Výše úvěru" (AddCondition (Condition_Amount (AmountCondition (LessThan 0)))) RemoveAmountCondition (amountForm conditions.amount) ]

                _ ->
                    []
    in
    div [] <|
        [ conditionRow "Úrok" (AddCondition (Condition_Amount (AmountCondition (LessThan 0)))) RemoveAmountCondition interestForm
        , conditionRow "Účel úvěru" (AddCondition (Condition_Amount (AmountCondition (LessThan 0)))) RemoveAmountCondition purposeForm
        , conditionRow "Délka úvěru" (AddCondition (Condition_Amount (AmountCondition (LessThan 0)))) RemoveAmountCondition termForm
        , conditionRow "Zdroj příjmů klienta" (AddCondition (Condition_Amount (AmountCondition (LessThan 0)))) RemoveAmountCondition mainIncomeForm
        , conditionRow "Příběh" (AddCondition (Condition_Amount (AmountCondition (LessThan 0)))) RemoveAmountCondition storyForm
        , conditionRow "Kraj klienta" (AddCondition (Condition_Amount (AmountCondition (LessThan 0)))) RemoveAmountCondition regionForm
        ]
            ++ amountRowOnlyEnabledForLoans


ratingForm : Html ModalMsg
ratingForm =
    text "abc"


amountForm : Maybe AmountCondition -> Html ModalMsg
amountForm mc =
    case mc of
        Nothing ->
            text ""

        Just (AmountCondition amt) ->
            Html.map AmountMsg <| Amount.amountForm amt


updateAmount : AmountMsg -> MarketplaceFilter -> MarketplaceFilter
updateAmount msg (MarketplaceFilter f) =
    let
        { ignoreWhen } =
            f

        newIgnoreWhen =
            { ignoreWhen | amount = Maybe.map (Amount.map (Amount.update msg)) ignoreWhen.amount }
    in
    MarketplaceFilter { f | ignoreWhen = newIgnoreWhen }


interestForm : Html ModalMsg
interestForm =
    text "ghi"


purposeForm : Html ModalMsg
purposeForm =
    text "jkl"


termForm : Html ModalMsg
termForm =
    text "mno"


mainIncomeForm : Html ModalMsg
mainIncomeForm =
    text "pqr"


storyForm : Html ModalMsg
storyForm =
    text "stu"


regionForm : Html ModalMsg
regionForm =
    text "vwx"


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
