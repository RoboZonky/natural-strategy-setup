module Data.Confirmation exposing
    ( ConfirmationSettings
    , confirmationsDisabled
    , decoder
    , encode
    , equal
    , render
    )

import Data.Filter.Conditions.Rating as Rating exposing (RatingCondition(..))
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


equal : ConfirmationSettings -> ConfirmationSettings -> Bool
equal (RatingList cs1) (RatingList cs2) =
    let
        makeComparable =
            List.sort << List.map Rating.ratingToString
    in
    makeComparable cs1 == makeComparable cs2



--JSON


encode : ConfirmationSettings -> Value
encode =
    Rating.encodeCondition


decoder : Decoder ConfirmationSettings
decoder =
    Rating.conditionDecoder
