module CTreeZipper exposing (Zipper)

import CTree


type alias Crumb d =
    { datum : d
    , before : List (CTree.Tree d)
    , after : List (CTree.Tree d)
    }


type alias ZipperModel d =
    { focus : CTree.Tree d
    , before : List (CTree.Tree d)
    , after : List (CTree.Tree d)
    , crumbs : List Crumb
    }


type Zipper d
    = Zipper (ZipperModel d)


fromTree t =
    ZipperModel t []
