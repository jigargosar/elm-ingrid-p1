module CTreeZipper exposing (Zipper, appendGoL, appendGoR, firstChild, fromTree, tree)

import CTree exposing (Tree)
import Html exposing (div)
import Pivot exposing (Pivot)
import V exposing (cx, t)


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


type alias MaybeZipper d =
    Maybe (Zipper d)


type alias MaybeZipperModel d =
    Maybe (ZipperModel d)


wrap =
    Zipper


unwrap (Zipper zm) =
    zm


map fn =
    unwrap >> fn >> wrap


mapMaybe : (ZipperModel d -> MaybeZipperModel d) -> Zipper d -> MaybeZipper d
mapMaybe fn =
    unwrap >> fn >> Maybe.map wrap


fromTree : Tree d -> Zipper d
fromTree t =
    Zipper
        { pivot = Pivot.singleton t
        , crumbs = []
        }


pivot : Zipper d -> TreePivot d
pivot =
    unwrap >> .pivot


tree : Zipper d -> Tree d
tree =
    pivot >> Pivot.getC


appendGoR : Tree d -> Zipper d -> Zipper d
appendGoR t =
    map (\zm -> { zm | pivot = Pivot.appendGoR t zm.pivot })


appendGoL : Tree d -> Zipper d -> Zipper d
appendGoL t =
    map (\zm -> { zm | pivot = Pivot.appendGoL t zm.pivot })


firstChild : Zipper d -> MaybeZipper d
firstChild =
    mapMaybe
        (\zm ->
            Pivot.getC zm.pivot
                |> CTree.children
                |> Pivot.fromList
                |> Maybe.map (\np -> { zm | pivot = np, crumbs = zm.pivot :: zm.crumbs })
        )


main =
    div [ cx [] ] [ t "HW" ]
