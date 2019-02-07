module View.Filter.DeletionModal exposing
    ( ChangeToConfirm(..)
    , Model
    , UserDecision(..)
    , askForConfirmation
    , init
    , update
    , view
    )

import Bootstrap.Button as Button
import Bootstrap.Modal as Modal
import Data.Filter as Filter exposing (BuyingConfiguration, MarketplaceFilter)
import Html exposing (Html, text)
import Types exposing (DeletionModalMsg(..))


init : Model
init =
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

        CancelDeletion ->
            let
                userChoice =
                    case model.changeToConfirm of
                        BuyingConfigChange previous _ ->
                            RestorePreviousBuying previous

                        NoChange ->
                            OkToDelete
            in
            ( { model | openCloseState = Modal.hidden }, Just userChoice )


type ChangeToConfirm
    = BuyingConfigChange BuyingConfiguration BuyingConfiguration
    | NoChange


type UserDecision
    = RestorePreviousBuying BuyingConfiguration
    | OkToDelete


view : Model -> Html DeletionModalMsg
view { changeToConfirm, openCloseState } =
    Modal.config CancelDeletion
        |> Modal.large
        |> Modal.h5 [] [ text "Potvrďte odstranění filtrů" ]
        |> Modal.body [] [ modalBody changeToConfirm ]
        |> Modal.footer []
            [ Button.button
                [ Button.danger
                , Button.onClick ConfirmDeletion
                ]
                [ text "Ano, odstranit" ]
            , Button.button
                [ Button.success
                , Button.onClick CancelDeletion
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
