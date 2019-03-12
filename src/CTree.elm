module CTree exposing (CTree, Forest, datum, forest, fromDatum)


type alias Forest a =
    List (CTree a)


emptyForest =
    []


type CTree a
    = CTree a (Forest a)


fromDatum d =
    CTree d emptyForest


datum (CTree d _) =
    d


forest (CTree _ f) =
    f
