module ItemTree exposing
    ( ItemTreeCursor
    , initialCursor
    )

import Array exposing (Array)


type alias Fragment =
    String


type Node
    = Node
        { fragment : Fragment
        , collapsed : Bool
        , children : Array Node
        }


type alias Path =
    Array Int


type alias ItemTreeCursor =
    { root : Node, path : Path }


initialRoot : Node
initialRoot =
    Node { fragment = "Root", collapsed = False, children = Array.empty }


rootPath : Path
rootPath =
    Array.empty


initialCursor : ItemTreeCursor
initialCursor =
    { root = initialRoot, path = rootPath }


createEmptyNode : Node
createEmptyNode =
    Node { fragment = "Empty", collapsed = False, children = Array.empty }
