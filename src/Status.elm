module Status exposing
    ( Status
    , decoder
    , fromString
    , toString
    )

import Json.Decode as JD exposing (Decoder)



---------------------------------------------------------------
-- TYPES --
---------------------------------------------------------------


type Status
    = Status String



---------------------------------------------------------------
-- API --
---------------------------------------------------------------


fromString : String -> Status
fromString str =
    Status str


toString : Status -> String
toString (Status str) =
    str


decoder : Decoder Status
decoder =
    JD.string
        |> JD.map fromString
