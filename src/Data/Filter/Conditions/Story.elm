module Data.Filter.Conditions.Story
    exposing
        ( Story(..)
        , StoryCondition(..)
        , StoryMsg
        , conditionDecoder
        , defaultCondition
        , encodeCondition
        , form
        , renderCondition
        , update
        )

import Bootstrap.Form.Radio as Radio
import Html exposing (Html, div)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util


type Story
    = SHORT
    | BELOW_AVERAGE
    | AVERAGE
    | ABOVE_AVERAGE


allStories : List Story
allStories =
    [ SHORT, BELOW_AVERAGE, AVERAGE, ABOVE_AVERAGE ]


type StoryCondition
    = StoryCondition Story


defaultCondition : StoryCondition
defaultCondition =
    StoryCondition SHORT


storyToString : Story -> String
storyToString story =
    case story of
        SHORT ->
            "velmi krátký"

        BELOW_AVERAGE ->
            "kratší než průměrný"

        AVERAGE ->
            "průměrně dlouhý"

        ABOVE_AVERAGE ->
            "delší než průměrný"


renderCondition : StoryCondition -> String
renderCondition (StoryCondition story) =
    "příběh je " ++ storyToString story


type StoryMsg
    = SetStory Story


update : StoryMsg -> StoryCondition -> StoryCondition
update (SetStory s) _ =
    StoryCondition s


form : StoryCondition -> Html StoryMsg
form (StoryCondition currentStory) =
    div [] <| List.map (storyRadio currentStory) allStories


storyRadio : Story -> Story -> Html StoryMsg
storyRadio currentStory thisRadiosStory =
    Radio.radio
        [ Radio.name "story"
        , Radio.checked <| currentStory == thisRadiosStory
        , Radio.onClick (SetStory thisRadiosStory)
        ]
        (storyToString thisRadiosStory)



-- JSON


encodeStory : Story -> Value
encodeStory =
    Encode.string << toString


encodeCondition : StoryCondition -> Value
encodeCondition (StoryCondition s) =
    encodeStory s


storyDecoder : Decoder Story
storyDecoder =
    Util.enumDecoder allStories


conditionDecoder : Decoder StoryCondition
conditionDecoder =
    Decode.map StoryCondition storyDecoder
