module View.Filter.FilterCreationModal exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Data.Filter as Filter exposing (FilteredItem(..), MarketplaceFilter(..), getFilteredItem, renderMarketplaceFilter, setFilteredItem)
import Data.Tooltip as Tooltip
import Html exposing (Html, div, hr, li, text, ul)
import Html.Attributes exposing (class, style, value)
import Html.Events exposing (onClick, onSubmit)
import Types exposing (..)
import View.Filter.Conditions as Conditions exposing (conditionsForm)
import View.Tooltip as Tooltip


type alias Model =
    { editedFilter : MarketplaceFilter
    , editingPositiveSubform : Bool
    , openCloseState : Modal.State
    }


initialState : Model
initialState =
    { editedFilter = Filter.emptyFilter
    , editingPositiveSubform = True
    , openCloseState = Modal.hiddenState
    }


update : ModalMsg -> Model -> ( Model, Maybe MarketplaceFilter )
update msg model =
    case msg of
        SaveFilter ->
            ( updateHelp msg model, Just model.editedFilter )

        _ ->
            ( updateHelp msg model, Nothing )


{-| Inner modal messages that don't produce Filter to be added to the main app's model
-}
updateHelp : ModalMsg -> Model -> Model
updateHelp msg model =
    case msg of
        ModalStateMsg filteredItem st ->
            { model | editedFilter = setFilteredItem filteredItem Filter.emptyFilter, openCloseState = st }

        TogglePositiveNegativeSubform ->
            { model | editingPositiveSubform = not model.editingPositiveSubform }

        PositiveConditionsChange condMsg ->
            { model | editedFilter = Filter.updatePositiveConditions (Conditions.update condMsg) model.editedFilter }

        NegativeConditionsChange condMsg ->
            { model | editedFilter = Filter.updateNegativeConditions (Conditions.update condMsg) model.editedFilter }

        ModalTooltipMsg tipId tooltipState ->
            {- This case is handled at the level of Main's update -}
            model

        SaveFilter ->
            { model | editedFilter = Filter.emptyFilter, openCloseState = Modal.hiddenState }

        ModalNoOp ->
            model


view : Model -> Tooltip.States -> Html ModalMsg
view { editedFilter, openCloseState, editingPositiveSubform } tooltipStates =
    let
        exceptionButtonText =
            if editingPositiveSubform then
                "Přidat Výjimku >>"
            else
                "<< Zpět"

        stateChangeMsg =
            ModalStateMsg <| getFilteredItem editedFilter
    in
    Modal.config stateChangeMsg
        |> Modal.large
        |> Modal.h5 []
            [ text "Vytvořit filtr"
            , Tooltip.popoverTipForModal Tooltip.filterCreationTip tooltipStates
            ]
        |> Modal.body []
            [ modalBody editedFilter editingPositiveSubform ]
        |> Modal.footer []
            [ Button.button
                [ Button.danger
                , Button.attrs [ onClick (stateChangeMsg Modal.hiddenState) ]
                ]
                [ text "Zrušit" ]
            , Button.button
                [ Button.success
                , Button.disabled (not <| Filter.isValid editedFilter)
                , Button.attrs [ onClick SaveFilter ]
                ]
                [ text "Uložit" ]
            , Button.button
                [ Button.secondary
                , Button.attrs [ onClick TogglePositiveNegativeSubform ]
                ]
                [ text exceptionButtonText ]
            ]
        |> Modal.view openCloseState


modalBody : MarketplaceFilter -> Bool -> Html ModalMsg
modalBody ((MarketplaceFilter filter) as mf) editingPositiveSubform =
    let
        validationErrors =
            Filter.marketplaceFilterValidationErrors mf

        previewOrValidationErrors =
            if List.isEmpty validationErrors then
                text <| renderMarketplaceFilter mf
            else
                ul [ style [ ( "color", "red" ) ] ] <|
                    List.map (\e -> li [] [ text e ]) validationErrors

        conditionsSubform =
            if editingPositiveSubform then
                div []
                    [ text <| "Nastavte podmínky filtru. " ++ Filter.itemToPluralString filter.whatToFilter ++ " splňující všechny podmínky budou ignorovány."
                    , Html.map PositiveConditionsChange <| conditionsForm filter.whatToFilter filter.ignoreWhen
                    ]
            else
                div []
                    [ text <| "Nastavte výjimku. " ++ Filter.itemToPluralString filter.whatToFilter ++ " splňující všechny podmínky výjimky NEBUDOU filtrem odstraněny."
                    , Html.map NegativeConditionsChange <| conditionsForm filter.whatToFilter filter.butNotWhen
                    ]
    in
    Grid.containerFluid []
        [ Grid.row []
            [ Grid.col
                [ Col.xs12 ]
                [ conditionsSubform
                , hr [] []
                , previewOrValidationErrors
                ]
            ]
        ]
