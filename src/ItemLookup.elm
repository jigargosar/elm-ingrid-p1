module ItemLookup exposing
    ( Item
    , ItemLookup
    , fromList
    , getById
    , getChildrenOfId
    , getParentAndGrandParentOf
    , getParentAndPrevPrevSibOf
    , getParentOfId
    , getPrevSiblingOfId
    , getRoot
    , getRootItems
    , insertAll
    , toList
    )

import Array exposing (Array)
import Dict exposing (Dict)
import List.Extra


type alias Item =
    { id : String
    , rev : Maybe String
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


toParentIdLookup : ItemLookup -> Dict String String
toParentIdLookup itemLookup =
    toList itemLookup
        |> List.concatMap (\item -> List.map (\cid -> ( cid, item.id )) item.childIds)
        |> Dict.fromList


insertAll : List Item -> ItemLookup -> ItemLookup
insertAll items itemLookup =
    List.foldl (\item -> Dict.insert item.id item) itemLookup items


getRoot : ItemLookup -> Maybe Item
getRoot itemLookup =
    getById rootItemId itemLookup


getById : String -> ItemLookup -> Maybe Item
getById id itemLookup =
    Dict.get id itemLookup


getParentOfId : String -> ItemLookup -> Maybe Item
getParentOfId id itemLookup =
    let
        parentIdLookup : Dict String String
        parentIdLookup =
            toParentIdLookup itemLookup
    in
    getById id itemLookup
        |> Maybe.andThen
            ((\item -> Dict.get item.id parentIdLookup)
                >> Maybe.andThen (\pid -> getById pid itemLookup)
            )



--getAncestorIds : String -> ItemLookup -> Maybe (List String)
--getAncestorIds id itemLookup =
--    getById id itemLookup
--        |> Maybe.map (\_ -> getAncestorIdsHelp [] id itemLookup)
--
--
--getAncestorIdsHelp : List String -> String -> ItemLookup -> List String
--getAncestorIdsHelp ancestorIds id itemLookup =
--    let
--        maybeParent : Maybe Item
--        maybeParent =
--            getParentOfId id itemLookup
--
--        newAncestorIds : List String
--        newAncestorIds =
--            id :: ancestorIds
--    in
--    case maybeParent of
--        Just parent ->
--            getAncestorIdsHelp newAncestorIds parent.id itemLookup
--
--        Nothing ->
--            newAncestorIds


rootItemId =
    "i_root_item_id"


getRootItems : ItemLookup -> Maybe (List Item)
getRootItems itemLookup =
    getChildrenOfId rootItemId itemLookup



--getRootItemsOrEmpty : ItemLookup -> (List Item)
--getRootItemsOrEmpty itemLookup =
--    getRootItems itemLookup |> Maybe.withDefault []


getChildrenOfId : String -> ItemLookup -> Maybe (List Item)
getChildrenOfId parentId itemLookup =
    getById parentId itemLookup |> Maybe.map (\parent -> List.filterMap (\cid -> getById cid itemLookup) parent.childIds)


getSiblingsOfId : String -> ItemLookup -> Maybe (List Item)
getSiblingsOfId id itemLookup =
    getParentOfId id itemLookup
        |> Maybe.andThen (\parent -> getChildrenOfId parent.id itemLookup)


getPrevSiblingOfId : String -> ItemLookup -> Maybe Item
getPrevSiblingOfId id itemLookup =
    getParentOfId id itemLookup
        |> Maybe.andThen
            (\parent ->
                parent.childIds
                    |> List.Extra.findIndex ((==) id)
                    |> Maybe.andThen (\idx -> parent.childIds |> List.Extra.getAt (idx - 1))
                    |> Maybe.andThen (\cid -> getById cid itemLookup)
            )


getParentAndPrevPrevSibOf : String -> ItemLookup -> Maybe ( String, Item, Item )
getParentAndPrevPrevSibOf id itemLookup =
    getParentOfId id itemLookup
        |> Debug.log "getParentOfId"
        |> Maybe.andThen
            (\parent ->
                parent.childIds
                    |> List.Extra.findIndex ((==) id)
                    |> Maybe.andThen (\idx -> parent.childIds |> List.Extra.getAt (idx - 1))
                    |> Maybe.andThen (\cid -> getById cid itemLookup)
                    |> Maybe.map (\prevSib -> ( id, parent, prevSib ))
            )


getParentAndGrandParentOf : String -> ItemLookup -> Maybe ( String, Item, Item )
getParentAndGrandParentOf id itemLookup =
    getParentOfId id itemLookup
        |> Maybe.andThen
            (\parent ->
                getParentOfId parent.id itemLookup
                    |> Maybe.map (\grandParent -> ( id, parent, grandParent ))
            )



--        |> Maybe.andThen
--            (\( parent, grandParent ) ->
--                grandParent.childIds
--                    |> List.Extra.findIndex ((==) parent.id)
--                    |> Maybe.map (\parentIdx -> ( id, ( parent, parentIdx ), grandParent ))
--            )
--
--        |> Array.
