module View.Filter.FilterCreationModal exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Data.Filter as Filter exposing (FilteredItem(..), MarketplaceFilter(..), setFilteredItem)
import Html exposing (Html, text)
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
                [ whatToFilterForm ]
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
