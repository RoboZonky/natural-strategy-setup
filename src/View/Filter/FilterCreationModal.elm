module View.Filter.FilterCreationModal exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Data.Filter as Filter exposing (FilteredItem(..), MarketplaceFilter(..), getFilteredItem, renderBuyFilter, renderSellFilter, setFilteredItem)
import Data.Tooltip as Tooltip
import Html exposing (Html, div, hr, li, text, ul)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
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
    let
        newModel =
            updateHelp msg model
    in
    case msg of
        SaveFilter ->
            ( newModel, Just model.editedFilter )

        _ ->
            ( newModel, Nothing )


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

        ModalTooltipMsg _ _ ->
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

        ( modalTitle, tooltipKey ) =
            case getFilteredItem editedFilter of
                Participation_To_Sell ->
                    ( "Vytvořit pravidlo pro prodej", Tooltip.sellFilterCreationTip )

                _ ->
                    ( "Vytvořit filtr", Tooltip.buyFilterCreationTip )

        stateChangeMsg =
            ModalStateMsg <| getFilteredItem editedFilter
    in
    Modal.config stateChangeMsg
        |> Modal.large
        |> Modal.h5 []
            [ text modalTitle
            , Tooltip.popoverTipForModal tooltipKey tooltipStates
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

        filterRenderer =
            case getFilteredItem mf of
                Participation_To_Sell ->
                    renderSellFilter

                _ ->
                    renderBuyFilter

        previewOrValidationErrors =
            if List.isEmpty validationErrors then
                text <| filterRenderer mf
            else
                ul [ style [ ( "color", "red" ) ] ] <|
                    List.map (\e -> li [] [ text e ]) validationErrors

        conditionsSubform =
            if editingPositiveSubform then
                Html.map PositiveConditionsChange <| conditionsForm filter.whatToFilter filter.ignoreWhen
            else
                Html.map NegativeConditionsChange <| conditionsForm filter.whatToFilter filter.butNotWhen
    in
    Grid.containerFluid []
        [ Grid.row []
            [ Grid.col
                [ Col.xs12 ]
                [ div []
                    [ text <| whatToDoText filter.whatToFilter editingPositiveSubform
                    , conditionsSubform
                    ]
                , hr [] []
                , previewOrValidationErrors
                ]
            ]
        ]


whatToDoText : FilteredItem -> Bool -> String
whatToDoText filteredItem editingPositiveSubform =
    case ( filteredItem, editingPositiveSubform ) of
        ( Participation_To_Sell, True ) ->
            "Nastavte podmínky pravidla pro prodej Participací. Participace splňující všechny podmínky budou prodány."

        ( Participation_To_Sell, False ) ->
            "Nastavte výjimku pravidla pro prodej Participací. Participace splňující všechny podmínky výjimky NEBUDOU prodány."

        ( _, True ) ->
            "Nastavte podmínky filtru. " ++ Filter.itemToPluralString filteredItem ++ " splňující všechny podmínky budou ignorovány."

        ( _, False ) ->
            "Nastavte výjimku. " ++ Filter.itemToPluralString filteredItem ++ " splňující všechny podmínky výjimky NEBUDOU filtrem ignorovány."
