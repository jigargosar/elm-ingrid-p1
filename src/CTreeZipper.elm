module CTreeZipper exposing (Zipper, appendGoL, appendGoR, fromTree, tree)

import CTree exposing (Tree)
import Pivot exposing (Pivot)


type alias Crumb d =
    { datum : d
    , before : List (Tree d)
    , after : List (Tree d)
    }


type alias TreePivot d =
    Pivot (Tree d)


type alias ZipperModel d =
    { pivot : TreePivot d
    , crumbs : List (TreePivot d)
    }


type Zipper d
    = Zipper (ZipperModel d)


wrap =
    Zipper


unwrap (Zipper zm) =
    zm


map fn =
    unwrap >> fn >> wrap


fromTree : Tree d -> Zipper d
fromTree t =
    Zipper
        { pivot = Pivot.singleton t
        , crumbs = []
        }


tree : Zipper d -> Tree d
tree (Zipper zm) =
    Pivot.getC zm.pivot


appendGoR : Tree d -> Zipper d -> Zipper d
appendGoR t =
    map (\zm -> { zm | pivot = Pivot.appendGoR t zm.pivot })


appendGoL : Tree d -> Zipper d -> Zipper d
appendGoL t =
    map (\zm -> { zm | pivot = Pivot.appendGoL t zm.pivot })
