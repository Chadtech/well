module Assignee exposing
    ( Assignee
    , decoder
    , fromString
    , toString
    )

import Json.Decode as JD exposing (Decoder)



---------------------------------------------------------------
-- TYPES --
---------------------------------------------------------------


type Assignee
    = Assignee String



---------------------------------------------------------------
-- API --
---------------------------------------------------------------


fromString : String -> Assignee
fromString str =
    Assignee str


toString : Assignee -> String
toString (Assignee str) =
    str


decoder : Decoder Assignee
decoder =
    JD.string
        |> JD.map fromString
