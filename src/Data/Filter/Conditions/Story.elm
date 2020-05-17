module Data.Filter.Conditions.Story exposing
    ( Story(..)
    , StoryCondition(..)
    , conditionDecoder
    , defaultCondition
    , encodeCondition
    , form
    , renderCondition
    )

import Bootstrap.Form.Radio as Radio
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Util


type Story
    = SHORT
    | BELOW_AVERAGE
    | AVERAGE
    | ABOVE_AVERAGE


allStories : List Story
allStories =
    [ SHORT
    , BELOW_AVERAGE
    , AVERAGE
    , ABOVE_AVERAGE
    ]


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


form : StoryCondition -> Html StoryCondition
form (StoryCondition currentStory) =
    allStories
        |> List.indexedMap (\index story -> storyRadio index currentStory story)
        |> Html.div []


storyRadio : Int -> Story -> Story -> Html StoryCondition
storyRadio index currentStory thisRadiosStory =
    Radio.radio
        [ Radio.id ("story_" ++ String.fromInt index)
        , Radio.name "story"
        , Radio.checked (currentStory == thisRadiosStory)
        , Radio.onClick (StoryCondition thisRadiosStory)
        ]
        (storyToString thisRadiosStory)


encodeStory : Story -> Value
encodeStory =
    Util.enumEncoder allStories


encodeCondition : StoryCondition -> Value
encodeCondition (StoryCondition s) =
    encodeStory s


storyDecoder : Decoder Story
storyDecoder =
    Util.enumDecoder "Story" allStories


conditionDecoder : Decoder StoryCondition
conditionDecoder =
    Decode.map StoryCondition storyDecoder
