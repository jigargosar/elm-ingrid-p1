module Item.Zipper exposing (ItemCursor, ItemZipper)

import Item exposing (Item)
import Tree.Zipper exposing (Zipper)


type alias ItemZipper =
    Zipper Item


type alias ItemCursor =
    ItemZipper
