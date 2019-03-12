module CTree exposing (Tree, children, datum, fromDatum, mapDatum)


type alias Children d =
    List (Tree d)


type alias TreeModel d =
    { datum : d
    , children : Children d
    }


type Tree d
    = Tree (TreeModel d)


wrap =
    Tree


unwrap (Tree model) =
    model


map : (TreeModel d -> TreeModel d) -> Tree d -> Tree d
map fn =
    unwrap >> fn >> wrap


fromDatum : d -> Tree d
fromDatum d =
    TreeModel d [] |> wrap


datum : Tree d -> d
datum =
    unwrap >> .datum


children : Tree d -> Children d
children =
    unwrap >> .children


mapDatum : (d -> d) -> Tree d -> Tree d
mapDatum fn =
    map (\tm -> { tm | datum = fn tm.datum })
