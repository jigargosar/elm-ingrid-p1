module Item exposing
    ( Item
    , ItemLookup
    , fromList
    , toList
    )

import Dict exposing (Dict)


type alias Item =
    { id : String
    , title : String
    , pid : Maybe String
    , childIds : List String
    }


type alias ItemLookup =
    Dict String Item


fromList : List Item -> ItemLookup
fromList itemList =
    itemList
        |> List.map (\item -> ( item.id, item ))
        |> Dict.fromList


toList : ItemLookup -> List Item
toList itemLookup =
    itemLookup |> Dict.values
