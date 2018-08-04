module View.Filter.FilterDeletionModal
    exposing
        ( ChangeToConfirm(..)
        , Model
        , UserDecision(..)
        , askForConfirmation
        , initClosed
        , update
        , view
        )

import Bootstrap.Button as Button
import Bootstrap.Modal as Modal
import Data.Filter as Filter exposing (BuyingConfiguration, MarketplaceFilter, SellingConfiguration)
import Html exposing (Html, text)
import Html.Events exposing (onClick)
import Types exposing (DeletionModalMsg(..))


initClosed : Model
initClosed =
    { changeToConfirm = NoChange
    , openCloseState = Modal.hidden
    }


askForConfirmation : ChangeToConfirm -> Model
askForConfirmation change =
    { changeToConfirm = change
    , openCloseState = Modal.shown
    }


type alias Model =
    { changeToConfirm : ChangeToConfirm
    , openCloseState : Modal.Visibility
    }


update : DeletionModalMsg -> Model -> ( Model, Maybe UserDecision )
update msg model =
    case msg of
        ConfirmDeletion ->
            ( { model | openCloseState = Modal.hidden }, Just OkToDelete )

        DeletionModalStateMsg state ->
            let
                userChoice =
                    case model.changeToConfirm of
                        BuyingConfigChange previous _ ->
                            RestorePreviousBuying previous

                        SellingConfigChange previous _ ->
                            RestorePreviousSelling previous

                        NoChange ->
                            OkToDelete
            in
            ( { model | openCloseState = state }, Just userChoice )


type ChangeToConfirm
    = BuyingConfigChange BuyingConfiguration BuyingConfiguration
    | SellingConfigChange SellingConfiguration SellingConfiguration
    | NoChange


type UserDecision
    = OkToDelete
    | RestorePreviousBuying BuyingConfiguration
    | RestorePreviousSelling SellingConfiguration


view : Model -> Html DeletionModalMsg
view { changeToConfirm, openCloseState } =
    Modal.config (DeletionModalStateMsg Modal.hidden)
        |> Modal.large
        |> Modal.h5 [] [ text "Potvrďte odstranění filtrů" ]
        |> Modal.body [] [ modalBody changeToConfirm ]
        |> Modal.footer []
            [ Button.button
                [ Button.danger
                , Button.attrs [ onClick ConfirmDeletion ]
                ]
                [ text "Ano, odstranit" ]
            , Button.button
                [ Button.success
                , Button.attrs [ onClick (DeletionModalStateMsg Modal.hidden) ]
                ]
                [ text "Zrušit změnu" ]
            ]
        |> Modal.view openCloseState


modalBody : ChangeToConfirm -> Html a
modalBody change =
    let
        ( listOfFiltersToBeRemoved, itemBeingChanged ) =
            case change of
                BuyingConfigChange from to ->
                    ( Filter.getFiltersRemovedByBuyingConfigurationChange from to, "nákupu" )

                SellingConfigChange from to ->
                    ( Filter.getFiltersRemovedBySellingConfigurationChange from to, "prodeje" )

                NoChange ->
                    ( [], "" )
    in
    Html.div []
        [ text <| "Tato změna pravidel " ++ itemBeingChanged ++ " vyžaduje odstranění následujících filtrů"
        , Html.ul [] <|
            List.map viewFilter listOfFiltersToBeRemoved
        ]


viewFilter : MarketplaceFilter -> Html a
viewFilter mf =
    Html.li [] [ Filter.filterTextView mf ]
