module View.Filter.FilterCreationModal exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Html exposing (Html, text)
import Html.Events exposing (onClick)
import Types exposing (..)


view : Modal.State -> Html Msg
view modalState =
    Modal.config ModalMsg
        |> Modal.large
        |> Modal.h5 [] [ text "Vytvořit filtr" ]
        |> Modal.body []
            [ Grid.containerFluid []
                [ Grid.row []
                    [ Grid.col
                        [ Col.xs12 ]
                        [ text "Tělo modalu" ]
                    ]
                ]
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.primary
                , Button.attrs [ onClick <| ModalMsg Modal.hiddenState ]
                ]
                [ text "Přidat" ]
            , Button.button
                [ Button.danger
                , Button.attrs [ onClick <| ModalMsg Modal.hiddenState ]
                ]
                [ text "Zrušit" ]
            ]
        |> Modal.view modalState
