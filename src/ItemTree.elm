module ItemTree exposing (Fragment, Node)


type alias Fragment =
    String


type Node
    = Node
        { fragment : Fragment
        , collapsed : Bool
        , children : List Node
        }


type alias Path =
    List Int


root : Node
root =
    Node { fragment = "Root", collapsed = False, children = [] }
