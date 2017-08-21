module View.RatingCondition exposing (ratingCheckboxes)

import Data.Rating as Rating exposing (Rating, RatingCondition)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick)
import Types exposing (..)


ratingCheckboxes : RatingCondition -> Html Msg
ratingCheckboxes condition =
    Rating.allRatings
        |> List.map (\r -> ratingCheckbox r (Rating.ratingSatisfiesCondition condition r))
        |> div []


ratingCheckbox : Rating -> Bool -> Html Msg
ratingCheckbox rating isEnabled =
    label []
        [ input [ type_ "checkbox", onClick (ToggleNotificationOnRating rating (not isEnabled)) ] []
        , text <| Rating.ratingToString rating
        ]
