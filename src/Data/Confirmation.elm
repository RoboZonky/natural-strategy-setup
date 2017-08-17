module Data.Confirmation
    exposing
        ( Confirmation(..)
        , renderConfirmation
        )

import Data.Rating as Rating exposing (RatingCondition)


type Confirmation
    = Disabled
    | WhenRatingIs RatingCondition


renderConfirmation : Confirmation -> String
renderConfirmation confirmation =
    case confirmation of
        WhenRatingIs condition ->
            "Potvrzovat mobilem investice do úvěrů, kde " ++ Rating.renderRatingCondition condition ++ "."

        Disabled ->
            ""
