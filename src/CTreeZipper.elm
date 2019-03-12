module CTreeZipper exposing (Zipper)

import CTree exposing (Tree)


type alias ZipperModel d =
    { root : Tree d, cursor : List Int }


type Zipper d
    = Zipper (ZipperModel d)
