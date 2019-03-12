module CTreeZipper exposing (Zipper, fromTree)

import CTree
import Pivot exposing (Pivot)


type alias Crumb d =
    { datum : d
    , before : List (CTree.Tree d)
    , after : List (CTree.Tree d)
    }


type alias ZipperModel d =
    { pivot : Pivot d
    , crumbs : List (Pivot d)
    }


type Zipper d
    = Zipper (ZipperModel d)


fromTree : CTree.Tree d -> Zipper d
fromTree t =
    Zipper
        { pivot = Pivot.singleton t
        , crumbs = []
        }
