module Data.Confirmation
    exposing
        ( ConfirmationSettings
        , confirmationsDisabled
        , decoder
        , encode
        , render
        )

import Data.Filter.Conditions.Rating as Rating exposing (RatingCondition(RatingList))
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


type alias ConfirmationSettings =
    RatingCondition


confirmationsDisabled : ConfirmationSettings
confirmationsDisabled =
    RatingList []


render : ConfirmationSettings -> String
render (RatingList enabledRatings) =
    if List.isEmpty enabledRatings then
        ""
    else
        "Potvrzovat mobilem investice do úvěrů, kde " ++ Rating.renderCondition (RatingList enabledRatings) ++ "."



--JSON


encode : ConfirmationSettings -> Value
encode =
    Rating.encodeCondition


decoder : Decoder ConfirmationSettings
decoder =
    Rating.conditionDecoder
