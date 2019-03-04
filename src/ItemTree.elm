module ItemTree exposing
    ( ItemTreeCursor
    , currentRoot
    , initialCursor
    , isSelected
    , nodeFragment
    )

import Array exposing (Array)


type alias Fragment =
    String


type ItemTreeNode
    = Node
        { fragment : Fragment
        , collapsed : Bool
        , children : Array ItemTreeNode
        }


type alias Path =
    Array Int


type alias ItemTreeCursor =
    { root : ItemTreeNode, path : Path }


initialRoot : ItemTreeNode
initialRoot =
    Node { fragment = "Root", collapsed = False, children = Array.empty }


rootPath : Path
rootPath =
    Array.empty


initialCursor : ItemTreeCursor
initialCursor =
    { root = initialRoot, path = rootPath }


currentRoot : ItemTreeCursor -> ItemTreeNode
currentRoot cursor =
    cursor.root


nodeFragment : ItemTreeNode -> Fragment
nodeFragment (Node { fragment }) =
    fragment


isSelected : ItemTreeNode -> ItemTreeCursor -> Bool
isSelected node cursor =
    True


createEmptyNode : ItemTreeNode
createEmptyNode =
    Node { fragment = "Empty", collapsed = False, children = Array.empty }
