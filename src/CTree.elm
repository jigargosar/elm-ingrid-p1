module CTree exposing (Children, Tree, children, datum, fromDatum)


type alias Children d =
    List (Tree d)


type alias TreeModel d =
    { d : d
    , c : Children d
    }


type Tree d
    = Tree (TreeModel d)


wrap =
    Tree


unwrap (Tree model) =
    model


fromDatum : d -> Tree d
fromDatum d =
    TreeModel d [] |> wrap


datum : Tree d -> d
datum =
    unwrap >> .d


children : Tree d -> Children d
children =
    unwrap >> .c
