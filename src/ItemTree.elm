module ItemTree exposing
    ( Fragment
    , Node(..)
    , Path
    , root
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


root : Node
root =
    Node { fragment = "Root", collapsed = False, children = Array.empty }


rootPath : Path
rootPath =
    Array.empty


createEmptyNode : Node
createEmptyNode =
    Node { fragment = "Empty", collapsed = False, children = Array.empty }
