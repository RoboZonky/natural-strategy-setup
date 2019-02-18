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
import Data.Validate as Validate
import DomId exposing (DomId)
import Html exposing (Html, text)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra
import View.EnumSelect as EnumSelect exposing (DefaultOptionConfig(..))


type Interest
    = LessThan Rating
    | Between Rating Rating
    | MoreThan Rating
    | Exactly Rating


type InterestEnum
    = LT
    | BTW
    | MT
    | EX


type InterestCondition
    = InterestCondition Interest


defaultCondition : InterestCondition
defaultCondition =
    InterestCondition (MoreThan Rating.A)


toString : Interest -> String
toString interest =
    case interest of
        LessThan maxBound ->
            "nedosahuje " ++ showInterestPercent maxBound

        Between minBound maxBound ->
            "je " ++ showInterest minBound ++ " až " ++ showInterestPercent maxBound

        MoreThan minBound ->
            "přesahuje " ++ showInterestPercent minBound

        Exactly val ->
            "je " ++ showInterestPercent val


renderCondition : InterestCondition -> String
renderCondition (InterestCondition interest) =
    "úrok " ++ toString interest


validationErrors : InterestCondition -> List String
validationErrors (InterestCondition ic) =
    case ic of
        Between minBound maxBound ->
            validateMinLtMax minBound maxBound

        LessThan _ ->
            []

        MoreThan _ ->
            []

        Exactly _ ->
            []


validateMinLtMax : Rating -> Rating -> List String
validateMinLtMax minBound maxBound =
    Validate.validate (Rating.toInterestPercent minBound >= Rating.toInterestPercent maxBound)
        "Úrok: minimum musí být menší než maximum"


type InterestMsg
    = SetLessThan Rating
    | SetBetween Rating Rating
    | SetMoreThan Rating
    | SetExactly Rating
    | InterestNoOp


toEnum : Interest -> InterestEnum
toEnum interest =
    case interest of
        Exactly _ ->
            EX

        LessThan _ ->
            LT

        Between _ _ ->
            BTW

        MoreThan _ ->
            MT


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

            SetExactly val ->
                Exactly val

            InterestNoOp ->
                i


type alias InterestDropdowns =
    { extactly : Html InterestMsg
    , lessThan : Html InterestMsg
    , between : ( Html InterestMsg, Html InterestMsg )
    , moreThan : Html InterestMsg
    }


defaultDropdowns : InterestDropdowns
defaultDropdowns =
    InterestDropdowns disabledDropdown disabledDropdown ( disabledDropdown, disabledDropdown ) disabledDropdown


form : InterestCondition -> Html InterestMsg
form (InterestCondition interest) =
    let
        interestEnum =
            toEnum interest

        dropdowns =
            case interest of
                Exactly x ->
                    { defaultDropdowns | extactly = ratingDropdown True (DefaultOption x) SetExactly }

                LessThan x ->
                    { defaultDropdowns | lessThan = ratingDropdownWithValues allRatingsExceptSmallest True (DefaultOption x) SetLessThan }

                Between mi ma ->
                    { defaultDropdowns
                        | between =
                            ( ratingDropdown True (DefaultOption mi) (\mi1 -> SetBetween mi1 ma)
                            , ratingDropdown True (DefaultOption ma) (\ma1 -> SetBetween mi ma1)
                            )
                    }

                MoreThan x ->
                    { defaultDropdowns | moreThan = ratingDropdownWithValues allRatingsExceptLargest True (DefaultOption x) SetMoreThan }
    in
    Html.div []
        [ Form.formInline [ onSubmit InterestNoOp ]
            [ interestRadio (interestEnum == EX) (SetExactly Rating.D) "je přesně\u{00A0}" "interest0"
            , dropdowns.extactly
            , unit
            ]
        , Form.formInline [ onSubmit InterestNoOp ]
            [ interestRadio (interestEnum == LT) (SetLessThan Rating.D) "nedosahuje\u{00A0}" "interest1"
            , dropdowns.lessThan
            , unit
            ]
        , Form.formInline [ onSubmit InterestNoOp ]
            [ interestRadio (interestEnum == BTW) (SetBetween Rating.A_Double_Star Rating.D) "je\u{00A0}" "interest2"
            , Tuple.first dropdowns.between
            , text "\u{00A0}až\u{00A0}"
            , Tuple.second dropdowns.between
            , unit
            ]
        , Form.formInline [ onSubmit InterestNoOp ]
            [ interestRadio (interestEnum == MT) (SetMoreThan Rating.A_Double_Star) "přesahuje\u{00A0}" "interest3"
            , dropdowns.moreThan
            , unit
            ]
        ]


unit : Html msg
unit =
    text "\u{00A0}% p.a."


ratingDropdown : Bool -> DefaultOptionConfig Rating -> (Rating -> InterestMsg) -> Html InterestMsg
ratingDropdown =
    ratingDropdownWithValues Rating.allRatings


ratingDropdownWithValues : List Rating -> Bool -> DefaultOptionConfig Rating -> (Rating -> InterestMsg) -> Html InterestMsg
ratingDropdownWithValues ratings enabled defaultOption toMsg =
    EnumSelect.from
        { enumValues = ratings
        , valuePickedMessage = toMsg
        , showVisibleLabel = Rating.showInterest
        , defaultOption = defaultOption
        , enabled = enabled
        }


allRatingsExceptLargest : List Rating
allRatingsExceptLargest =
    List.Extra.init Rating.allRatings
        -- won't happen, as allRatings is fixed non-empty list of ratings
        |> Maybe.withDefault []


allRatingsExceptSmallest : List Rating
allRatingsExceptSmallest =
    List.tail Rating.allRatings
        -- won't happen, as allRatings is fixed non-empty list of ratings
        |> Maybe.withDefault []


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

        Exactly x ->
            Encode.object [ ( "v", Encode.int 4 ), ( "w", encodeRating x ) ]


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

                    4 ->
                        Decode.map Exactly
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
