module ItemTree exposing (Fragment, Node)


type alias Fragment =
    String


type Node
    = Node
        { fragment : Fragment
        , collapsed : Bool
        , children : List Node
        }


root : Node
root =
    Node { fragment = "Root", collapsed = False, children = [] }
