module View.RatingCondition exposing (ratingCheckboxes)

import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Data.Rating as Rating exposing (Rating, RatingCondition)
import Html exposing (Html, div, input, label, text)
import Html.Events exposing (onSubmit)
import Types exposing (..)


ratingCheckboxes : RatingCondition -> Html Msg
ratingCheckboxes condition =
    Rating.allRatings
        |> List.map (\r -> ratingCheckbox r (Rating.ratingSatisfiesCondition condition r))
        |> Form.formInline [ onSubmit NoOp ]


ratingCheckbox : Rating -> Bool -> Html Msg
ratingCheckbox rating isEnabled =
    Checkbox.checkbox
        [ Checkbox.onCheck (ToggleNotificationOnRating rating)
        , Checkbox.checked isEnabled
        , Checkbox.inline
        ]
        (Rating.ratingToString rating)
