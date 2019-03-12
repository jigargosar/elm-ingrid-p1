module CTreeZipper exposing (Zipper)

import CTree


type alias ZipperModel d =
    { root : CTree.Tree d, cursor : List Int }


type Zipper d
    = Zipper (ZipperModel d)


fromTree t =
    ZipperModel t []
