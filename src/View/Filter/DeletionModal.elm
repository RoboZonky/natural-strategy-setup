module View.Filter.DeletionModal exposing
    ( Config
    , Model
    , confirmBuyFiltersDeletion
    , confirmSellFiltersDeletion
    , init
    , view
    )

import Bootstrap.Button as Button
import Bootstrap.Modal as Modal
import Data.Filter as Filter exposing (BuyingConfiguration, MarketplaceFilter, SellingConfiguration)
import Html exposing (Html)


type alias Model =
    { changeToConfirm : Maybe ChangeToConfirm
    , openCloseState : Modal.Visibility
    }


type alias Config msg =
    { setBuyingConfig : BuyingConfiguration -> msg
    , setSellingConfig : SellingConfiguration -> msg
    }


type ChangeToConfirm
    = BuyingConfigChange BuyingConfiguration BuyingConfiguration (List MarketplaceFilter)
    | SellingConfigChange SellingConfiguration SellingConfiguration (List MarketplaceFilter)


init : Model
init =
    { changeToConfirm = Nothing
    , openCloseState = Modal.hidden
    }


open : ChangeToConfirm -> Model
open change =
    { changeToConfirm = Just change
    , openCloseState = Modal.shown
    }


confirmSellFiltersDeletion : SellingConfiguration -> SellingConfiguration -> Model
confirmSellFiltersDeletion old new =
    case Filter.getFiltersRemovedBySellingConfigurationChange old new of
        [] ->
            init

        filtersToRemove ->
            open (SellingConfigChange old new filtersToRemove)


confirmBuyFiltersDeletion : BuyingConfiguration -> BuyingConfiguration -> Model
confirmBuyFiltersDeletion old new =
    case Filter.getFiltersRemovedByBuyingConfigurationChange old new of
        [] ->
            init

        filtersToRemove ->
            open (BuyingConfigChange old new filtersToRemove)


cancelDeletion : Config msg -> ChangeToConfirm -> msg
cancelDeletion config change =
    case change of
        BuyingConfigChange old _ _ ->
            config.setBuyingConfig old

        SellingConfigChange old _ _ ->
            config.setSellingConfig old


confirmDeletion : Config msg -> ChangeToConfirm -> msg
confirmDeletion config change =
    case change of
        BuyingConfigChange _ new _ ->
            config.setBuyingConfig new

        SellingConfigChange _ new _ ->
            config.setSellingConfig new


view : Config msg -> Model -> Html msg
view config { changeToConfirm, openCloseState } =
    case changeToConfirm of
        Just change ->
            Modal.config (cancelDeletion config change)
                |> Modal.large
                |> Modal.h5 [] [ Html.text "Potvrďte odstranění filtrů" ]
                |> Modal.body [] [ modalBody change ]
                |> Modal.footer []
                    [ Button.button
                        [ Button.danger
                        , Button.onClick (confirmDeletion config change)
                        ]
                        [ Html.text "Ano, odstranit" ]
                    , Button.button
                        [ Button.success
                        , Button.onClick (cancelDeletion config change)
                        ]
                        [ Html.text "Zrušit změnu" ]
                    ]
                |> Modal.view openCloseState

        Nothing ->
            Html.text ""


modalBody : ChangeToConfirm -> Html a
modalBody change =
    case change of
        BuyingConfigChange _ _ removedFilters ->
            viewFiltersToBeDeleted removedFilters "nákupu"

        SellingConfigChange _ _ removedFilters ->
            viewFiltersToBeDeleted removedFilters "prodeje"


viewFiltersToBeDeleted : List MarketplaceFilter -> String -> Html a
viewFiltersToBeDeleted filters itemBeingChanged =
    Html.div []
        [ Html.text <| "Tato změna pravidel " ++ itemBeingChanged ++ " vyžaduje odstranění následujících filtrů"
        , Html.ul [] <|
            List.map viewFilter filters
        ]


viewFilter : MarketplaceFilter -> Html a
viewFilter mf =
    Html.li [] [ Filter.filterTextView mf ]
