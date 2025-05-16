module Ext.UniqueList exposing
    ( UniqueList
    , any
    , cons
    , empty
    , first
    , fromList
    , member
    , remove
    , toList
    , toggle
    )

import Ext.List as ListExt



----------------------------------------------------------------
-- TYPES --
----------------------------------------------------------------


type UniqueList v
    = UniqueList (List v)



----------------------------------------------------------------
-- API --
----------------------------------------------------------------


first : UniqueList v -> Maybe v
first (UniqueList list) =
    case list of
        [] ->
            Nothing

        x :: _ ->
            Just x


empty : UniqueList v
empty =
    UniqueList []


fromList : List v -> UniqueList v
fromList =
    UniqueList << ListExt.unique


member : v -> UniqueList v -> Bool
member value (UniqueList list) =
    List.member value list


toList : UniqueList v -> List v
toList (UniqueList list) =
    list


cons : v -> UniqueList v -> UniqueList v
cons value (UniqueList list) =
    fromList (value :: list)


remove : v -> UniqueList v -> UniqueList v
remove value (UniqueList list) =
    UniqueList (List.filter ((/=) value) list)


any : (v -> Bool) -> UniqueList v -> Bool
any predicate (UniqueList list) =
    List.any predicate list


toggle : v -> UniqueList v -> UniqueList v
toggle value set =
    if member value set then
        remove value set

    else
        cons value set
