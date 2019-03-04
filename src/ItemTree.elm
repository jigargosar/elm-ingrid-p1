module ItemTree exposing
    ( ItemTreeCursor
    , append
    , initialCursor
    , isSelected
    , nodeFragment
    , rootTree
    , selectedTree
    , treeChildren
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


rootTree : ItemTreeCursor -> ItemTree
rootTree =
    Tree.Zipper.root >> Tree.Zipper.tree


nodeFragment : ItemTree -> Fragment
nodeFragment =
    Tree.label >> .fragment


selectedTree : ItemTreeCursor -> ItemTree
selectedTree =
    Tree.Zipper.tree


isSelected : ItemTree -> ItemTreeCursor -> Bool
isSelected node cursor =
    True


prependChild : ItemTreeCursor -> ItemTreeCursor
prependChild cursor =
    let
        updatedTreeNode =
            selectedTree cursor
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
    if selectedTree cursor == rootTree cursor then
        prependChild cursor

    else
        appendSibling cursor


createEmptyNode : ItemTree
createEmptyNode =
    Tree.singleton { fragment = "Empty", collapsed = False }


treeChildren : ItemTree -> List ItemTree
treeChildren tree =
    Tree.children tree
