module ItemTree exposing
    ( ItemTreeCursor
    , appendEmptyFragment
    , currentRoot
    , initialCursor
    , isSelected
    , nodeFragment
    , selectedNode
    )

import Array exposing (Array)
import Tree
import Tree.Zipper


type alias Fragment =
    String


type alias NodeModel =
    { fragment : Fragment
    , collapsed : Bool
    }


type alias ItemTree =
    Tree.Tree NodeModel


type alias ItemTreeCursor =
    Tree.Zipper.Zipper NodeModel


initialRoot : ItemTree
initialRoot =
    Tree.singleton { fragment = "Root", collapsed = False }


initialCursor : ItemTreeCursor
initialCursor =
    Tree.Zipper.fromTree initialRoot


currentRoot : ItemTreeCursor -> ItemTree
currentRoot =
    Tree.Zipper.root >> Tree.Zipper.tree


nodeFragment : ItemTree -> Fragment
nodeFragment =
    Tree.label >> .fragment


selectedNode : ItemTreeCursor -> ItemTree
selectedNode =
    Tree.Zipper.tree


isSelected : ItemTree -> ItemTreeCursor -> Bool
isSelected node cursor =
    True


prependChild cursor =
    let
        parent =
            selectedNode cursor

        newParent =
            parent
    in
    cursor


appendSibling cursor =
    cursor


appendEmptyFragment : ItemTreeCursor -> ItemTreeCursor
appendEmptyFragment cursor =
    if selectedNode cursor == currentRoot cursor then
        prependChild cursor

    else
        appendSibling cursor


createEmptyNode : ItemTree
createEmptyNode =
    Tree.singleton { fragment = "Empty", collapsed = False }
