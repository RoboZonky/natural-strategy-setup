module Data.Tooltip
    exposing
        ( States
        , TipId
        , buyFilterCreationTip
        , buyFilterListTip
        , confirmationTip
        , getState
        , getTooltipText
        , initialStates
        , portfolioStructureTip
        , sellFilterCreationTip
        , sellFilterListTip
        , update
        )

import Bootstrap.Popover as Popover
import Dict exposing (Dict)


type TipId
    = TipId Int


knownTooltips : List ( TipId, String )
knownTooltips =
    [ ( portfolioStructureTip, "Struktura portfolia definuje požadované rozložení zůstatkové částky do úvěrů na základě ratingu. Můžete zvolit jedno ze tří předdefinovaných portfolií (konzervativní, balancované, progresivní) nebo zvolte prázné a vyplňte požadované procentuální podíly sami." )
    , ( confirmationTip, "Úvěry s ratingem A, B, C a D jsou velmi žádané a v prvních minutách po uvedení na tržiště jsou chráněny CAPTCHA. Pokud pro ně nezapnete mobilní notifikace, robotovi se je s největší pravděpodobností nepodaří zainvestovat." )
    , ( buyFilterListTip, "Pravidla pro nákup umožňují ignorovat některé položky (úvěry či participace) na tržišti. Daná položka bude ignorována pokud splní podmínky alespoň jednoho z Vámi definovaných pravidel." )
    , ( sellFilterListTip, "Pravidla pro prodej určují které Vámi vlastněné participace má robot prodávat. Daná participace bude prodána pokud splní podmínky alespoň jednoho z Vámi definovaných pravidel." )
    , ( buyFilterCreationTip, "Pravidla pro nákup určují které položky na tržišti (úvěry či participace) mají být ignorovány. Daná položka bude ignorována pokud splní všechny podmínky pravidla. Pokud však zároveň splní všechny podmínky výjimky, ignorována nebude." )
    , ( sellFilterCreationTip, "Pravidla pro prodej určují které participace má robot prodávat. Daná participace bude prodána pokud splní všechny podmínky pravidla. Pokud však zároveň splní všechny podmínky výjimky, prodána nebude." )
    ]


internalTipsDict : Dict Int String
internalTipsDict =
    Dict.fromList <| List.map (\( TipId x, tip ) -> ( x, tip )) knownTooltips


type States
    = States (Dict Int Popover.State)


initialStates : States
initialStates =
    States <| Dict.map (\_ _ -> Popover.initialState) internalTipsDict


getState : TipId -> States -> Popover.State
getState (TipId x) (States states) =
    Dict.get x states |> Maybe.withDefault Popover.initialState


update : TipId -> Popover.State -> States -> States
update (TipId x) s (States states) =
    States <| Dict.update x (Maybe.map (\_ -> s)) states


getTooltipText : TipId -> String
getTooltipText (TipId id) =
    Dict.get id internalTipsDict |> Maybe.withDefault "Nápověda není k dispozici"



-- Tooltip Ids


portfolioStructureTip : TipId
portfolioStructureTip =
    TipId 1


confirmationTip : TipId
confirmationTip =
    TipId 2


sellFilterListTip : TipId
sellFilterListTip =
    TipId 3


buyFilterListTip : TipId
buyFilterListTip =
    TipId 4


buyFilterCreationTip : TipId
buyFilterCreationTip =
    TipId 5


sellFilterCreationTip : TipId
sellFilterCreationTip =
    TipId 6
