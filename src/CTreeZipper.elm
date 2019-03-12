module CTreeZipper exposing (Zipper, fromTree, tree)

import CTree exposing (Tree)
import Pivot exposing (Pivot)


type alias Crumb d =
    { datum : d
    , before : List (Tree d)
    , after : List (Tree d)
    }


type alias ZipperModel d =
    { pivot : Pivot d
    , crumbs : List (Pivot d)
    }


type Zipper d
    = Zipper (ZipperModel d)


fromTree : Tree d -> Zipper d
fromTree t =
    Zipper
        { pivot = Pivot.singleton t
        , crumbs = []
        }


tree : Zipper d -> Tree d
tree (Zipper zm) =
    Pivot.getC zm.pivot
