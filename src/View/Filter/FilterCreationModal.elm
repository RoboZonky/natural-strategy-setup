module View.Filter.FilterCreationModal exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Bootstrap.Tab as Tab
import Data.Filter as Filter exposing (FilteredItem(..), MarketplaceFilter(..), renderMarketplaceFilter, setFilteredItem)
import Data.Filter.Conditions as Conditions exposing (..)
import Data.Tooltip as Tooltip
import Html exposing (Html, div, hr, li, text, ul)
import Html.Attributes exposing (class, style, value)
import Html.Events exposing (onClick, onSubmit)
import Types exposing (..)
import View.Filter.Conditions as Conditions exposing (conditionsForm)
import View.Tooltip as Tooltip


type alias Model =
    { editedFilter : MarketplaceFilter
    , withException : Bool
    , openCloseState : Modal.State
    , tabState : Tab.State
    }


initialState : Model
initialState =
    { editedFilter = Filter.emptyFilter
    , withException = False
    , openCloseState = Modal.hiddenState
    , tabState = Tab.initialState
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
            { state | editedFilter = Filter.updatePositiveConditions (removeAmountConditionIfNotFilteringLoan item) <| setFilteredItem item state.editedFilter }

        ModalStateMsg st ->
            { state | editedFilter = Filter.emptyFilter, openCloseState = st }

        PositiveConditionsChange condMsg ->
            { state | editedFilter = Filter.updatePositiveConditions (Conditions.update condMsg) state.editedFilter }

        NegativeConditionsChange condMsg ->
            { state | editedFilter = Filter.updateNegativeConditions (Conditions.update condMsg) state.editedFilter }

        ToggleException flag ->
            { state | withException = flag }

        ModalTooltipMsg tipId tooltipState ->
            {- This case is handled at the level of Main's update -}
            state

        TabMsg tabState ->
            { state | tabState = tabState }

        SaveFilter ->
            { state | editedFilter = Filter.emptyFilter, openCloseState = Modal.hiddenState }

        ModalNoOp ->
            state


view : Model -> Tooltip.States -> Html ModalMsg
view { editedFilter, openCloseState, tabState, withException } tooltipStates =
    Modal.config ModalStateMsg
        |> Modal.large
        |> Modal.h5 []
            [ text "Vytvořit filtr"
            , Tooltip.popoverTipForModal Tooltip.filterCreationTip tooltipStates
            ]
        |> Modal.body []
            [ modalBody editedFilter withException tabState ]
        |> Modal.footer []
            [ Checkbox.checkbox
                [ Checkbox.onCheck ToggleException
                , Checkbox.checked withException
                , Checkbox.inline
                ]
                "Filtr s výjimkou"
            , Button.button
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


modalBody : MarketplaceFilter -> Bool -> Tab.State -> Html ModalMsg
modalBody ((MarketplaceFilter state) as mf) withException tabState =
    let
        validationErrors =
            Filter.marketplaceFilterValidationErrors mf

        previewOrValidationErrors =
            if List.isEmpty validationErrors then
                text <| renderMarketplaceFilter mf
            else
                ul [ style [ ( "color", "red" ) ] ] <|
                    List.map (\e -> li [] [ text e ]) validationErrors

        positiveTab =
            Tab.item
                { id = "tab1"
                , link = Tab.link [] [ text "Podmínky filtru" ]
                , pane = Tab.pane [] [ Html.map PositiveConditionsChange <| conditionsForm state.whatToFilter state.ignoreWhen ]
                }

        exceptionTab =
            Tab.item
                { id = "tab2"
                , link = Tab.link [] [ text "Podmínky výjimky" ]
                , pane = Tab.pane [] [ Html.map NegativeConditionsChange <| conditionsForm state.whatToFilter state.butNotWhen ]
                }

        tabItems =
            if withException then
                [ positiveTab, exceptionTab ]
            else
                [ positiveTab ]
    in
    Grid.containerFluid []
        [ Grid.row []
            [ Grid.col
                [ Col.xs12 ]
                [ whatToFilterForm
                , bodyWithTabs tabItems tabState
                , hr [] []
                , previewOrValidationErrors
                ]
            ]
        ]


bodyWithTabs : List (Tab.Item ModalMsg) -> Tab.State -> Html ModalMsg
bodyWithTabs tabItems tabState =
    Tab.config TabMsg
        |> Tab.withAnimation
        --|> Tab.center
        |> Tab.items tabItems
        |> Tab.view tabState


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


removeAmountConditionIfNotFilteringLoan : FilteredItem -> Conditions -> Conditions
removeAmountConditionIfNotFilteringLoan filteredItem cs =
    if filteredItem /= Loan then
        removeAmountCondition cs
    else
        cs
