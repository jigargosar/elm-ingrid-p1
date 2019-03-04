module ItemTree exposing
    ( ItemTreeCursor
    , append
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


prependChild : ItemTreeCursor -> ItemTreeCursor
prependChild cursor =
    let
        updatedTreeNode =
            selectedNode cursor
                |> Tree.prependChild createEmptyNode

        newZipper =
            Tree.Zipper.replaceTree updatedTreeNode cursor
    in
    Tree.Zipper.forward newZipper
        |> Maybe.withDefault newZipper


appendSibling cursor =
    cursor


append : ItemTreeCursor -> ItemTreeCursor
append cursor =
    if selectedNode cursor == currentRoot cursor then
        prependChild cursor

    else
        appendSibling cursor


createEmptyNode : ItemTree
createEmptyNode =
    Tree.singleton { fragment = "Empty", collapsed = False }
