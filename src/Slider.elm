module Slider
    exposing
        ( SliderStates
        , getSliderRangeFor
        , initialSliderStates
        , sliderChangeSubscription
        , updateSliders
        )

import AllDict exposing (AllDict)
import Data.Filter.Conditions.Rating as Rating exposing (Rating(..))
import Data.PortfolioStructure exposing (Share)
import RangeSlider exposing (RangeSlider, setDimensions, setExtents, setFormatter, setStepSize, setValues)
import Types


percentageSlider : RangeSlider
percentageSlider =
    RangeSlider.init
        |> setStepSize (Just 1.0)
        |> setFormatter (\value -> toString value ++ "%")
        |> setDimensions 300 57
        |> setExtents 0 100
        |> setValues 0 0


type alias SliderStates =
    AllDict Rating RangeSlider Int


initialSliderStates : SliderStates
initialSliderStates =
    AllDict.fromList Rating.hash <| List.map (\r -> ( r, percentageSlider )) Rating.allRatings


sliderChangeSubscription : SliderStates -> Sub Types.Msg
sliderChangeSubscription allSliderStates =
    AllDict.toList allSliderStates
        |> List.map (\( rtg, sliderState ) -> Sub.map (Types.ChangePortfolioSharePercentage rtg) (RangeSlider.subscriptions sliderState))
        |> Sub.batch


updateSliders : Rating -> RangeSlider.Msg -> SliderStates -> SliderStates
updateSliders rtg msg allSliderStates =
    AllDict.update rtg (Maybe.map (RangeSlider.update msg)) allSliderStates


getSliderRangeFor : Rating -> SliderStates -> Share
getSliderRangeFor rtg sliderStates =
    AllDict.get rtg sliderStates
        |> Maybe.withDefault percentageSlider
        |> RangeSlider.getValues
        |> (\( minFloat, maxFloat ) -> ( round minFloat, round maxFloat ))
