module Data.Migration.Migration exposing
    ( Migration
    , MigrationWarning
    , andThen
    )


type alias MigrationWarning =
    String


{-| Migration ~ WriterT MigrationWarning
-}
type alias Migration a =
    ( a, List MigrationWarning )


andThen : (a -> Migration b) -> Migration a -> Migration b
andThen step ( a, warnings0 ) =
    let
        ( b, warnings1 ) =
            step a
    in
    ( b, warnings0 ++ warnings1 )
