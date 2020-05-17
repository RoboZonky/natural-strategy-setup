module View.Filter.CreationModal exposing (Model, init, update, view)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Radio as Radio
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Bootstrap.Utilities.Spacing as Spacing
import Data.Filter as Filter exposing (FilteredItem(..), MarketplaceFilter, setFilteredItem)
import Data.Filter.Complexity exposing (FilterComplexity(..))
import Data.Filter.Conditions exposing (ConditionType, Conditions, getEnabledConditionTypes, removeConditions)
import Data.Tooltip as Tooltip
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onSubmit)
import Types exposing (CreationModalMsg(..))
import View.Filter.Conditions as Conditions
import View.Tooltip as Tooltip


type alias Model =
    { editedFilter : MarketplaceFilter
    , allowedFilteredItems : List FilteredItem
    , filterComplexity : FilterComplexity
    , editingPositiveSubform : Bool
    , openCloseState : Modal.Visibility

    -- Switching FilteredItem might require removing conditions that users enabled that don't apply to the target FilteredItem
    , confirmRemoval : Maybe ( FilteredItem, List ConditionType, List ConditionType )
    }


init : Model
init =
    { editedFilter = Filter.emptyFilter
    , allowedFilteredItems = []
    , filterComplexity = Simple
    , editingPositiveSubform = True
    , openCloseState = Modal.hidden
    , confirmRemoval = Nothing
    }


update : CreationModalMsg -> Model -> ( Model, Maybe MarketplaceFilter )
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
updateHelp : CreationModalMsg -> Model -> Model
updateHelp msg model =
    case msg of
        OpenCreationModal filterComplexity allowedFilteredItems ->
            let
                filteredItem =
                    Maybe.withDefault Loan <| List.head allowedFilteredItems
            in
            { model
                | editedFilter = setFilteredItem filteredItem Filter.emptyFilter
                , allowedFilteredItems = allowedFilteredItems
                , filterComplexity = filterComplexity
                , editingPositiveSubform = True
                , openCloseState = Modal.shown
                , confirmRemoval = Nothing
            }

        SetFilteredItem filteredItem ->
            let
                conditionsToBeRemoved =
                    getConditionsRemovedByFilteredItemChange filteredItem model.editedFilter.ignoreWhen

                conditionsToBeRemovedFromException =
                    getConditionsRemovedByFilteredItemChange filteredItem model.editedFilter.butNotWhen
            in
            if List.isEmpty conditionsToBeRemoved && List.isEmpty conditionsToBeRemovedFromException then
                { model | editedFilter = setFilteredItem filteredItem model.editedFilter }

            else
                { model | confirmRemoval = Just ( filteredItem, conditionsToBeRemoved, conditionsToBeRemovedFromException ) }

        ConfirmConditionsRemoval ->
            case model.confirmRemoval of
                Nothing ->
                    model

                Just ( filteredItem, conditionsToBeRemoved, conditionsToBeRemovedFromException ) ->
                    let
                        updatedFilter =
                            { whatToFilter = filteredItem
                            , ignoreWhen = removeConditions conditionsToBeRemoved model.editedFilter.ignoreWhen
                            , butNotWhen = removeConditions conditionsToBeRemovedFromException model.editedFilter.butNotWhen
                            }
                    in
                    { model
                        | editedFilter = updatedFilter
                        , confirmRemoval = Nothing
                    }

        CancelConditionsRemoval ->
            { model | confirmRemoval = Nothing }

        TogglePositiveNegativeSubform ->
            { model | editingPositiveSubform = not model.editingPositiveSubform }

        PositiveConditionsChange condMsg ->
            { model | editedFilter = Filter.updatePositiveConditions (Conditions.update condMsg) model.editedFilter }

        NegativeConditionsChange condMsg ->
            { model | editedFilter = Filter.updateNegativeConditions (Conditions.update condMsg) model.editedFilter }

        SaveFilter ->
            { model | openCloseState = Modal.hidden }

        CloseModal ->
            { model | openCloseState = Modal.hidden }

        ModalTooltipMsg _ _ ->
            {- This case is handled at the level of Main's update -}
            model

        ModalNoOp ->
            model


view : Model -> Tooltip.States -> Html CreationModalMsg
view { editedFilter, openCloseState, editingPositiveSubform, allowedFilteredItems, filterComplexity, confirmRemoval } tooltipStates =
    let
        ( modalTitle, tooltipKey ) =
            case editedFilter.whatToFilter of
                Participation_To_Sell ->
                    ( "Vytvořit pravidlo prodeje", Tooltip.sellFilterCreationTip )

                _ ->
                    ( "Vytvořit pravidlo nákupu", Tooltip.buyFilterCreationTip )

        ( modalBody, modalFooter ) =
            case confirmRemoval of
                Nothing ->
                    ( marketplaceFilterEditor editedFilter filterComplexity editingPositiveSubform allowedFilteredItems
                    , [ Button.button
                            [ Button.danger
                            , Button.onClick CloseModal
                            ]
                            [ Html.text "Zrušit" ]
                      , Button.button
                            [ Button.success
                            , Button.disabled (not <| Filter.isValid editedFilter)
                            , Button.onClick SaveFilter
                            ]
                            [ Html.text "Uložit" ]
                      , exceptionButtonWhenFilterComplex filterComplexity editingPositiveSubform
                      ]
                    )

                Just ( filteredItem, conditionsToBeRemoved, conditionsToBeRemovedFromException ) ->
                    ( askForConfirmationOfRemoval filteredItem conditionsToBeRemoved conditionsToBeRemovedFromException
                    , [ Button.button
                            [ Button.success
                            , Button.onClick ConfirmConditionsRemoval
                            ]
                            [ Html.text "Ano, provést změnu" ]
                      , Button.button
                            [ Button.danger
                            , Button.onClick CancelConditionsRemoval
                            ]
                            [ Html.text "Ne, zrušit změnu" ]
                      ]
                    )
    in
    Modal.config CloseModal
        |> Modal.large
        |> Modal.h5 []
            [ Html.text modalTitle
            , Tooltip.popoverTipForModal tooltipKey tooltipStates
            ]
        |> Modal.body [] [ modalBody ]
        |> Modal.footer [] modalFooter
        |> Modal.view openCloseState


askForConfirmationOfRemoval : FilteredItem -> List ConditionType -> List ConditionType -> Html CreationModalMsg
askForConfirmationOfRemoval filteredItem conditionsToBeRemoved conditionsToBeRemovedFromException =
    Html.div []
        [ Html.text <|
            "Chystáte se změnit pravidlo, aby sloužilo k filtrování "
                ++ Filter.itemToPluralStringGenitive filteredItem
                ++ ". Tato změna vyžaduje odstranění následujících podmínek pravidla či výjimky, které jste dříve přidali:"
        , Html.ul [] <|
            List.map (\condToRemove -> Html.li [] [ Html.text <| Conditions.getVisibleLabel filteredItem condToRemove ])
                (conditionsToBeRemoved ++ conditionsToBeRemovedFromException)
        ]


{-| Simple filters don't allow exception definition
-}
exceptionButtonWhenFilterComplex : FilterComplexity -> Bool -> Html CreationModalMsg
exceptionButtonWhenFilterComplex filterComplexity editingPositiveSubform =
    let
        exceptionButtonText =
            if editingPositiveSubform then
                "Přidat Výjimku >>"

            else
                "<< Zpět"
    in
    case filterComplexity of
        Simple ->
            Html.text ""

        Complex ->
            Button.button
                [ Button.secondary
                , Button.onClick TogglePositiveNegativeSubform
                ]
                [ Html.text exceptionButtonText ]


marketplaceFilterEditor : MarketplaceFilter -> FilterComplexity -> Bool -> List FilteredItem -> Html CreationModalMsg
marketplaceFilterEditor mf filterComplexity editingPositiveSubform allowedFilteredItems =
    let
        validationErrors =
            Filter.marketplaceFilterValidationErrors mf

        previewOrValidationErrors =
            if List.isEmpty validationErrors then
                Filter.filterTextView mf

            else
                Html.ul [ style "color" "red" ] <|
                    List.map (\e -> Html.li [] [ Html.text e ]) validationErrors

        conditionsSubform =
            if editingPositiveSubform then
                Html.map PositiveConditionsChange <| Conditions.form filterComplexity mf.whatToFilter mf.ignoreWhen

            else
                Html.map NegativeConditionsChange <| Conditions.form filterComplexity mf.whatToFilter mf.butNotWhen
    in
    Grid.containerFluid []
        [ Grid.row []
            [ Grid.col
                [ Col.xs12 ]
                [ Html.div []
                    [ filteredItemRadios allowedFilteredItems mf.whatToFilter
                    , conditionsOrExceptionTitle editingPositiveSubform
                    , conditionsSubform
                    ]
                , Html.hr [] []
                , previewOrValidationErrors
                ]
            ]
        ]


filteredItemRadios : List FilteredItem -> FilteredItem -> Html CreationModalMsg
filteredItemRadios allowedFilteredItems currentFilteredItem =
    case allowedFilteredItems of
        [] ->
            Html.text ""

        [ _ {- only one item - just toString it without showing radios -} ] ->
            Html.text <|
                case currentFilteredItem of
                    Participation_To_Sell ->
                        "Pravidlo pro prodej " ++ Filter.itemToPluralStringGenitive currentFilteredItem

                    _ ->
                        "Pravidlo pro nákup " ++ Filter.itemToPluralStringGenitive currentFilteredItem

        moreFilteredItems ->
            Form.formInline [ onSubmit ModalNoOp ]
                (Html.text "Pravidlo pro nákup "
                    :: List.indexedMap (filteredItemRadio currentFilteredItem) moreFilteredItems
                )


filteredItemRadio : FilteredItem -> Int -> FilteredItem -> Html CreationModalMsg
filteredItemRadio currentFilteredItem index filteredItem =
    Radio.radio
        [ Radio.id <| "filteredItem" ++ String.fromInt index
        , Radio.checked (currentFilteredItem == filteredItem)
        , Radio.name "filteredItem"
        , Radio.onClick <| SetFilteredItem filteredItem
        , Radio.attrs [ Spacing.mx1 ]
        ]
        (Filter.itemToPluralStringGenitive filteredItem)


conditionsOrExceptionTitle : Bool -> Html a
conditionsOrExceptionTitle editingPositiveSubform =
    Html.text <|
        if editingPositiveSubform then
            ""

        else
            " - podmínky výjimky"


{-| Calculate which ConditionType-s need to be removed when changing to new FilteredItem
-}
getConditionsRemovedByFilteredItemChange : FilteredItem -> Conditions -> List ConditionType
getConditionsRemovedByFilteredItemChange filteredItem conditions =
    let
        currentlyEnabled : List ConditionType
        currentlyEnabled =
            getEnabledConditionTypes conditions

        validForFilteredItem : List ConditionType
        validForFilteredItem =
            Conditions.conditionTypesThatApplyTo filteredItem
    in
    List.filter (\enabledCondType -> not <| List.member enabledCondType validForFilteredItem) currentlyEnabled
