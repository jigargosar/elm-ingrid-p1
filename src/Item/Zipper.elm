module Item.Zipper exposing (ItemCursor, ItemZipper, decoder, encoder)

import Item exposing (Item)
import Item.Tree
import Json.Decode exposing (Decoder)
import Json.Encode
import Tree.Zipper as Zipper exposing (Zipper)


type alias ItemZipper =
    Zipper Item


type alias ItemCursor =
    ItemZipper


encoder : ItemZipper -> Json.Encode.Value
encoder =
    Zipper.root >> Zipper.tree >> Item.Tree.encoder


decoder : Decoder ItemZipper
decoder =
    Item.Tree.decoder
        |> Json.Decode.map Zipper.fromTree
