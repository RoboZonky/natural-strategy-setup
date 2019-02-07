module Data.Filter.Conditions.Rating exposing
    ( Rating(..)
    , RatingCondition(..)
    , RatingMsg
    , allRatings
    , conditionDecoder
    , defaultCondition
    , encodeCondition
    , form
    , initRatingDict
    , ratingToString
    , renderCondition
    , update
    , validationErrors
    )

import Bootstrap.Form.Checkbox as Checkbox
import Dict.Any exposing (AnyDict)
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
    [ A_Double_Star
    , A_Star
    , A_Double_Plus
    , A_Plus
    , A
    , B
    , C
    , D
    ]


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


initRatingDict : List ( Rating, a ) -> AnyDict Int Rating a
initRatingDict =
    Dict.Any.fromList hash


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


toDomId : String -> Rating -> String
toDomId domIdPrefix rating =
    domIdPrefix ++ String.fromInt (hash rating)


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


form : String -> RatingCondition -> Html RatingMsg
form domIdPrefix
    {- domIdPrefix to generate unique id for checkboxes in
       1) confirmation settings 2) in rating conditions
    -}
    condition
    =
    allRatings
        |> List.map (\r -> ratingCheckbox domIdPrefix r (ratingSatisfiesCondition condition r))
        |> div []


ratingCheckbox : String -> Rating -> Bool -> Html RatingMsg
ratingCheckbox domIdPrefix rating isEnabled =
    Checkbox.checkbox
        [ Checkbox.id (toDomId domIdPrefix rating)
        , Checkbox.onCheck
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
    Util.enumEncoder allRatings


encodeCondition : RatingCondition -> Value
encodeCondition (RatingList rs) =
    Encode.list encodeRating rs


ratingDecoder : Decoder Rating
ratingDecoder =
    Util.enumDecoder "Rating" allRatings


conditionDecoder : Decoder RatingCondition
conditionDecoder =
    Decode.map RatingList <|
        Decode.list ratingDecoder
