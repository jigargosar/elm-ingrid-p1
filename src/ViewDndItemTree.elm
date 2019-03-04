module ViewDndItemTree exposing (getItemDomId, viewDndItemTree)

import DnDList
import Html exposing (Html, div)
import Html.Attributes exposing (id, tabindex)
import Html.Events exposing (onBlur, onFocus)
import Html.Keyed
import ItemLookup exposing (Item, ItemLookup)
import Tachyons exposing (classes)
import Tachyons.Classes exposing (..)
import V exposing (t)


type alias System msg =
    DnDList.System msg Item


type alias Config msg =
    { onFocusMsg : Item -> msg
    , onBlurMsg : Item -> msg
    , system : System msg
    }


getItemDomId : Item -> String
getItemDomId item =
    "item-dnd-id-" ++ item.id


viewDndItemTree : Config msg -> List Item -> DnDList.Draggable -> Html msg
viewDndItemTree config displayRootItems draggable =
    let
        getItemKey item =
            case config.system.draggedIndex draggable of
                Just _ ->
                    "dragging-" ++ item.id

                Nothing ->
                    item.id
    in
    div []
        [ Html.Keyed.node "div"
            [ classes [] ]
            (List.indexedMap
                (\idx item -> ( getItemKey item, viewDraggableItem config config.system draggable idx item ))
                displayRootItems
            )
        , viewDraggedItem config.system draggable displayRootItems
        ]


viewItem attrs item =
    viewItemWithTitle attrs item.title


viewItemWithTitle attrs title =
    div
        (classes [ pa3, ba, br1, mv2, b__black_50 ]
            :: tabindex 1
            :: attrs
        )
        [ t <| title ]


viewDraggableItem : Config msg -> DnDList.System msg Item -> DnDList.Draggable -> Int -> Item -> Html msg
viewDraggableItem { onFocusMsg, onBlurMsg } system draggable index item =
    let
        maybeDraggedIndex : Maybe Int
        maybeDraggedIndex =
            system.draggedIndex draggable
    in
    case maybeDraggedIndex of
        Nothing ->
            let
                itemDomId : String
                itemDomId =
                    getItemDomId item
            in
            div
                [ id itemDomId
                , classes [ flex, items_center, pa3, ba, br1, mv2, b__black_50 ]
                , tabindex 0
                , onFocus <| onFocusMsg item
                , onBlur <| onBlurMsg item
                ]
                [ div [ classes [ flex_grow_1, flex, flex_column ] ]
                    [ div [ classes [] ] [ t item.title ]
                    , div [] (List.map (\cid -> div [] [ t cid ]) item.childIds)
                    ]
                , div (classes [ "move" ] :: system.dragEvents index itemDomId) [ t "|||" ]
                ]

        Just draggedIndex ->
            if draggedIndex /= index then
                viewItem (system.dropEvents index) item

            else
                viewItemWithTitle [] "[---------]"


viewDraggedItem : DnDList.System msg Item -> DnDList.Draggable -> List Item -> Html.Html msg
viewDraggedItem system draggable items =
    let
        maybeDraggedItem : Maybe Item
        maybeDraggedItem =
            system.draggedIndex draggable
                |> Maybe.andThen (\index -> items |> List.drop index |> List.head)
    in
    case maybeDraggedItem of
        Just item ->
            div (system.draggedStyles draggable)
                [ viewItem [ classes [ bg_white, o_80 ] ] item
                ]

        Nothing ->
            Html.text ""
