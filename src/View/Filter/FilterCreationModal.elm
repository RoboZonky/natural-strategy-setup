module View.Filter.FilterCreationModal exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Data.Filter as Filter exposing (Conditions, FilteredItem(..), MarketplaceFilter(..), renderMarketplaceFilter, setFilteredItem)
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
                , positiveConditionsForm state.whatToFilter
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


positiveConditionsForm : FilteredItem -> Html ModalMsg
positiveConditionsForm filteredItem =
    let
        optionalAmountRow =
            case filteredItem of
                Loan ->
                    [ conditionRow "Výše úvěru" amountForm ]

                _ ->
                    []
    in
    div [] <|
        [ conditionRow "Úrok" interestForm
        , conditionRow "Účel úvěru" purposeForm
        , conditionRow "Délka úvěru" termForm
        , conditionRow "Zdroj příjmů klienta" mainIncomeForm
        , conditionRow "Příběh" storyForm
        , conditionRow "Kraj klienta" regionForm
        ]
            ++ optionalAmountRow


ratingForm : Html ModalMsg
ratingForm =
    text "abc"


amountForm : Html ModalMsg
amountForm =
    text "def"


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


conditionRow : String -> Html ModalMsg -> Html ModalMsg
conditionRow conditionName subform =
    Grid.row []
        [ Grid.col [ Col.xs3 ] [ Checkbox.checkbox [] conditionName ]
        , Grid.col [ Col.xs9 ] [ subform ]
        ]
