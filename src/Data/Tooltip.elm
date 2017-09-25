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
    , ( buyFilterListTip, "Filtry umožňují ignorovat některé úvěry (a participace) na tržišti. Daný úvěr (či participace) bude ignorován, pokud splní podmínky alespoň jednoho z Vámi definovaných filtrů." )
    , ( sellFilterListTip, "Pravidla definované v této sekci určují které Vámi vlastněné participace si přejete prodávat." )
    , ( buyFilterCreationTip, "Filtr se skládá z jedné nebo více podmínek určujících, kdy má být úvěr (či participace) na primárním tržišti ignorován. Daný úvěr (či participace) bude ignorován, pokud splní všechny podmínky, které zde zvolíte.  Pokud však zároveň splní všechny podmínky výjimky, ignorován nebude." )
    , ( sellFilterCreationTip, "Pravidlo pro prodej se skládá z jedné či více podmínek. Robot se bude prodávat participace, které splňují všechny zvolené podmínky." )
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
