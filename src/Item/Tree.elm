module Item.Tree exposing (ItemCursor, ItemTree, decoder, encoder)

import Item exposing (Item)
import Json.Decode exposing (Decoder)
import Json.Encode
import Tree exposing (Tree)
import Tree.Zipper exposing (Zipper)


type alias ItemTree =
    Tree Item


type alias ItemCursor =
    Zipper Item


encoder : ItemTree -> Json.Encode.Value
encoder tree =
    let
        item =
            Tree.label tree

        children =
            Tree.children tree
    in
    Json.Encode.object
        [ ( "label", Item.encoder item )
        , ( "children", Json.Encode.list encoder children )
        ]


decoder : Decoder ItemTree
decoder =
    Json.Decode.map2 (\label children -> Tree.singleton label |> Tree.replaceChildren children)
        (Json.Decode.field "label" Json.Decode.string)
        (Json.Decode.field "children" (Json.Decode.list decoder))
