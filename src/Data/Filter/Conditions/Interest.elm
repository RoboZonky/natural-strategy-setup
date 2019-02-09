module Data.Filter.Conditions.Interest exposing
    ( Interest(..)
    , InterestCondition(..)
    , InterestMsg
    , conditionDecoder
    , defaultCondition
    , encodeCondition
    , form
    , renderCondition
    , update
    , validationErrors
    )

import Bootstrap.Form as Form
import Bootstrap.Form.Radio as Radio
import Data.Filter.Conditions.Rating as Rating exposing (Rating, showInterest, showInterestPercent)
import DomId exposing (DomId)
import Html exposing (Html, text)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util
import View.EnumSelect as EnumSelect exposing (DefaultOptionConfig(..))


type Interest
    = LessThan Rating
    | Between Rating Rating
    | MoreThan Rating


type InterestCondition
    = InterestCondition Interest


defaultCondition : InterestCondition
defaultCondition =
    InterestCondition (LessThan Rating.D)


toString : Interest -> String
toString interest =
    case interest of
        LessThan maxBound ->
            "nedosahuje " ++ showInterestPercent maxBound

        Between minBound maxBound ->
            "je " ++ showInterest minBound ++ " až " ++ showInterestPercent maxBound

        MoreThan minBound ->
            "přesahuje " ++ showInterestPercent minBound


renderCondition : InterestCondition -> String
renderCondition (InterestCondition interest) =
    "úrok " ++ toString interest


validationErrors : InterestCondition -> List String
validationErrors (InterestCondition ic) =
    case ic of
        Between minBound maxBound ->
            validateMinNotGtMax minBound maxBound

        LessThan _ ->
            []

        MoreThan _ ->
            []


validateMinNotGtMax : Rating -> Rating -> List String
validateMinNotGtMax minBound maxBound =
    Util.validate (Rating.toInterestPercent minBound > Rating.toInterestPercent maxBound)
        "Úrok: minimum nesmí být větší než maximum"


type InterestMsg
    = SetLessThan Rating
    | SetBetween Rating Rating
    | SetMoreThan Rating
    | InterestNoOp


whichEnabled : Interest -> ( Bool, Bool, Bool )
whichEnabled interest =
    case interest of
        LessThan _ ->
            ( True, False, False )

        Between _ _ ->
            ( False, True, False )

        MoreThan _ ->
            ( False, False, True )


update : InterestMsg -> InterestCondition -> InterestCondition
update msg (InterestCondition i) =
    InterestCondition <|
        case msg of
            SetLessThan hi ->
                LessThan hi

            SetBetween lo hi ->
                Between lo hi

            SetMoreThan lo ->
                MoreThan lo

            InterestNoOp ->
                i


type alias InterestDropdowns =
    { lessThan : Html InterestMsg
    , betweenMin : Html InterestMsg
    , betweenMax : Html InterestMsg
    , moreThan : Html InterestMsg
    }


defaultDropdowns : InterestDropdowns
defaultDropdowns =
    InterestDropdowns disabledDropdown disabledDropdown disabledDropdown disabledDropdown


form : InterestCondition -> Html InterestMsg
form (InterestCondition interest) =
    let
        dropdowns =
            case interest of
                LessThan x ->
                    { defaultDropdowns | lessThan = ratingDropdown True (DefaultOption x) SetLessThan }

                Between mi ma ->
                    { defaultDropdowns
                        | betweenMin = ratingDropdown True (DefaultOption mi) (\mi1 -> SetBetween mi1 ma)
                        , betweenMax = ratingDropdown True (DefaultOption ma) (\ma1 -> SetBetween mi ma1)
                    }

                MoreThan x ->
                    { defaultDropdowns | moreThan = ratingDropdown True (DefaultOption x) SetMoreThan }

        ( ltEnabled, btwEnabled, mtEnabled ) =
            whichEnabled interest
    in
    Html.div []
        [ Form.formInline [ onSubmit InterestNoOp ]
            [ interestRadio ltEnabled (SetLessThan Rating.D) "nedosahuje\u{00A0}" "interest1"
            , dropdowns.lessThan
            , unit
            ]
        , Form.formInline [ onSubmit InterestNoOp ]
            [ interestRadio btwEnabled (SetBetween Rating.A_Double_Star Rating.D) "je\u{00A0}" "interest2"
            , dropdowns.betweenMin
            , text "\u{00A0}až\u{00A0}"
            , dropdowns.betweenMax
            , unit
            ]
        , Form.formInline [ onSubmit InterestNoOp ]
            [ interestRadio mtEnabled (SetMoreThan Rating.A_Double_Star) "přesahuje\u{00A0}" "interest3"
            , dropdowns.moreThan
            , unit
            ]
        ]


unit : Html msg
unit =
    text "\u{00A0}% p.a."


ratingDropdown : Bool -> DefaultOptionConfig Rating -> (Rating -> InterestMsg) -> Html InterestMsg
ratingDropdown enabled def toMsg =
    EnumSelect.from
        { enumValues = Rating.allRatings
        , valuePickedMessage = toMsg
        , showVisibleLabel = Rating.showInterest
        , defaultOption = def
        , enabled = enabled
        }


disabledDropdown : Html InterestMsg
disabledDropdown =
    ratingDropdown False (DummyOption "") (always InterestNoOp)


interestRadio : Bool -> InterestMsg -> String -> DomId -> Html InterestMsg
interestRadio checked msg label domId =
    Radio.radio
        [ Radio.id domId
        , Radio.name "interest"
        , Radio.checked checked
        , Radio.onClick msg
        ]
        label



-- JSON


encodeInterest : Interest -> Value
encodeInterest i =
    case i of
        LessThan x ->
            Encode.object [ ( "v", Encode.int 1 ), ( "w", encodeRating x ) ]

        Between x y ->
            Encode.object [ ( "v", Encode.int 2 ), ( "x", encodeRating x ), ( "y", encodeRating y ) ]

        MoreThan y ->
            Encode.object [ ( "v", Encode.int 3 ), ( "w", encodeRating y ) ]


encodeRating : Rating -> Value
encodeRating =
    Encode.int << Rating.hash


encodeCondition : InterestCondition -> Value
encodeCondition (InterestCondition c) =
    encodeInterest c


interestDecoder : Decoder Interest
interestDecoder =
    Decode.field "v" Decode.int
        |> Decode.andThen
            (\typ ->
                case typ of
                    1 ->
                        Decode.map LessThan
                            (Decode.field "w" Decode.int |> Decode.andThen ratingDecoder)

                    2 ->
                        Decode.map2 Between
                            (Decode.field "x" Decode.int |> Decode.andThen ratingDecoder)
                            (Decode.field "y" Decode.int |> Decode.andThen ratingDecoder)

                    3 ->
                        Decode.map MoreThan
                            (Decode.field "w" Decode.int |> Decode.andThen ratingDecoder)

                    _ ->
                        Decode.fail <| "Invalid interest type " ++ String.fromInt typ
            )


ratingDecoder : Int -> Decoder Rating.Rating
ratingDecoder hash =
    case Rating.fromHash hash of
        Nothing ->
            Decode.fail <| "Invalid Rating hash when decoding interest: " ++ String.fromInt hash

        Just r ->
            Decode.succeed r


conditionDecoder : Decoder InterestCondition
conditionDecoder =
    Decode.map InterestCondition interestDecoder
