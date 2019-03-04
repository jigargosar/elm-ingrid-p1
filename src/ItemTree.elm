module ItemTree exposing (Fragment, Node)

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
    List Int


root : Node
root =
    Node { fragment = "Root", collapsed = False, children = Array.empty }
