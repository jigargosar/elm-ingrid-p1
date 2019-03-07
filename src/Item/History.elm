module Item.History exposing (HistoryDoc)

import Item.Zipper exposing (ItemZipper)
import Time exposing (Posix)


type alias HistoryDoc =
    { id : String
    , pid : String
    , cursor : ItemZipper
    , cAt : Posix
    }
