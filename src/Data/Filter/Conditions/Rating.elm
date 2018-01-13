module Data.Filter.Conditions.Rating
    exposing
        ( Rating(..)
        , RatingCondition(..)
        , RatingMsg
        , allRatings
        , conditionDecoder
        , defaultCondition
        , encodeCondition
        , form
        , hash
        , ratingFromString
        , ratingToString
        , renderCondition
        , update
        , validationErrors
        )

import Bootstrap.Form.Checkbox as Checkbox
import Html exposing (Html, div)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Util


type Rating
    = A_Double_Star
    | A_Star
    | A_Double_Plus
    | A_Plus
    | A
    | B
    | C
    | D


allRatings : List Rating
allRatings =
    [ A_Double_Star, A_Star, A_Double_Plus, A_Plus, A, B, C, D ]


ratingToString : Rating -> String
ratingToString r =
    case r of
        A_Double_Star ->
            "A**"

        A_Star ->
            "A*"

        A_Double_Plus ->
            "A++"

        A_Plus ->
            "A+"

        A ->
            "A"

        B ->
            "B"

        C ->
            "C"

        D ->
            "D"


ratingFromString : String -> Maybe Rating
ratingFromString s =
    allRatings |> List.filter (\r -> toString r == s) |> List.head



-- TODO Elm 0.18 doesn't make it possible to use Union type values as keys in Dict
-- As a workaround using eeue56/elm-all-dict which makes that possible, but requires explicit key hashing function
-- Expecting this to be rectified in 0.19


hash : Rating -> Int
hash rating =
    case rating of
        A_Double_Star ->
            1

        A_Star ->
            2

        A_Double_Plus ->
            3

        A_Plus ->
            4

        A ->
            5

        B ->
            6

        C ->
            7

        D ->
            8


type RatingCondition
    = RatingList (List Rating)


defaultCondition : RatingCondition
defaultCondition =
    RatingList []


{-| For the purposes of conditionRendering we're simplifying list to "better than" or "worse than" if the values
in the list form continuous range from beginning / from end of enumerated Rating values
-}
type SimplifiedRatingCondition
    = SimplifiedRatingList (List Rating)
    | BetterThan Rating
    | WorseThan Rating


validationErrors : RatingCondition -> List String
validationErrors (RatingList rlist) =
    Util.validate (List.isEmpty rlist) "Rating: zvolte aspoň jeden"


renderCondition : RatingCondition -> String
renderCondition ratingCondition =
    let
        subExpr =
            case simplify ratingCondition of
                SimplifiedRatingList rs ->
                    renderRatingList rs

                BetterThan r ->
                    "lepší než " ++ ratingToString r

                WorseThan r ->
                    "horší než " ++ ratingToString r
    in
    "rating je " ++ subExpr


renderRatingList : List Rating -> String
renderRatingList =
    Util.orList ratingToString


simplify : RatingCondition -> SimplifiedRatingCondition
simplify (RatingList rlist) =
    let
        sortedHashes =
            List.sort <| List.map hash rlist

        len =
            List.length rlist

        ratingCount =
            List.length allRatings

        allHashes =
            List.range 1 ratingCount
    in
    if List.isPrefixOf sortedHashes allHashes && 0 < len && len < ratingCount then
        BetterThan <| Maybe.withDefault A_Double_Star <| List.head <| List.drop len allRatings
    else if List.isSuffixOf sortedHashes allHashes && 0 < len && len < ratingCount then
        WorseThan <| Maybe.withDefault D <| List.last <| List.take (ratingCount - len) allRatings
    else
        SimplifiedRatingList rlist


ratingSatisfiesCondition : RatingCondition -> Rating -> Bool
ratingSatisfiesCondition (RatingList rlist) rating =
    List.member rating rlist


type RatingMsg
    = AddRating Rating
    | RemoveRating Rating


update : RatingMsg -> RatingCondition -> RatingCondition
update msg (RatingList rlist) =
    case msg of
        AddRating r ->
            RatingList <| r :: rlist

        RemoveRating r ->
            RatingList <| List.filter (\rr -> rr /= r) rlist


form : RatingCondition -> Html RatingMsg
form condition =
    allRatings
        |> List.map (\r -> ratingCheckbox r (ratingSatisfiesCondition condition r))
        |> div []


ratingCheckbox : Rating -> Bool -> Html RatingMsg
ratingCheckbox rating isEnabled =
    Checkbox.checkbox
        [ Checkbox.onCheck
            (\checked ->
                if checked then
                    AddRating rating
                else
                    RemoveRating rating
            )
        , Checkbox.checked isEnabled
        , Checkbox.inline
        ]
        (ratingToString rating)



-- JSON


encodeRating : Rating -> Value
encodeRating =
    Encode.string << toString


encodeCondition : RatingCondition -> Value
encodeCondition (RatingList rs) =
    Encode.list <| List.map encodeRating rs


ratingDecoder : Decoder Rating
ratingDecoder =
    Util.enumDecoder allRatings


conditionDecoder : Decoder RatingCondition
conditionDecoder =
    Decode.map RatingList <|
        Decode.list ratingDecoder
