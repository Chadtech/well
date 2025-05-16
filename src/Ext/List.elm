module Ext.List exposing (..)


unique : List x -> List x
unique list =
    List.foldl
        (\x acc ->
            if List.member x acc then
                acc

            else
                x :: acc
        )
        []
        (List.reverse list)
