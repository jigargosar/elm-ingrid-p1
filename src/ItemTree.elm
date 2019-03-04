module ItemTree exposing
    ( ItemTreeCursor
    , appendNew
    , backward
    , forward
    , indent
    , initialCursor
    , outdent
    , prependNew
    , rootTree
    , selectedTree
    , treeChildren
    , treeFragment
    , treeId
    )

import Tree
import Tree.Zipper


type alias Fragment =
    String


type alias Item =
    { id : String
    , fragment : Fragment
    , collapsed : Bool
    }


type alias ItemTree =
    Tree.Tree Item


type alias ItemTreeCursor =
    Tree.Zipper.Zipper Item


initialRoot : ItemTree
initialRoot =
    Tree.singleton { id = "ROOT_ITEM_ID", fragment = "Root", collapsed = False }


initialCursor : ItemTreeCursor
initialCursor =
    Tree.Zipper.fromTree initialRoot


rootTree : ItemTreeCursor -> ItemTree
rootTree =
    Tree.Zipper.root >> Tree.Zipper.tree


treeFragment : ItemTree -> Fragment
treeFragment =
    Tree.label >> .fragment


treeId : ItemTree -> Fragment
treeId =
    Tree.label >> .id


selectedTree : ItemTreeCursor -> ItemTree
selectedTree =
    Tree.Zipper.tree


prependChild : ItemTree -> ItemTreeCursor -> ItemTreeCursor
prependChild newTree cursor =
    let
        updatedTreeNode =
            selectedTree cursor
                |> Tree.prependChild newTree

        newZipper =
            Tree.Zipper.replaceTree updatedTreeNode cursor
    in
    newZipper


appendNew : String -> ItemTreeCursor -> ItemTreeCursor
appendNew id cursor =
    let
        newTree =
            createEmptyNode id

        newZipper =
            if selectedTree cursor == rootTree cursor then
                prependChild newTree cursor

            else
                Tree.Zipper.append newTree cursor
    in
    Tree.Zipper.forward newZipper
        |> Maybe.withDefault newZipper


prependNew : String -> ItemTreeCursor -> ItemTreeCursor
prependNew id cursor =
    let
        newTree =
            createEmptyNode id

        newZipper =
            if selectedTree cursor == rootTree cursor then
                prependChild newTree cursor

            else
                Tree.Zipper.prepend newTree cursor
    in
    Tree.Zipper.forward newZipper
        |> Maybe.withDefault newZipper


createEmptyNode : String -> ItemTree
createEmptyNode id =
    Tree.singleton { id = id, fragment = "Empty", collapsed = False }


treeChildren : ItemTree -> List ItemTree
treeChildren tree =
    Tree.children tree


backward cursor =
    Tree.Zipper.backward cursor
        |> Maybe.withDefault cursor


forward cursor =
    Tree.Zipper.forward cursor
        |> Maybe.withDefault cursor


indent cursor =
    let
        selected =
            selectedTree cursor
    in
    Tree.Zipper.siblingsBeforeFocus cursor
        |> List.head
        |> Maybe.andThen
            (\prevSibTree ->
                Tree.Zipper.removeTree cursor
                    |> Maybe.andThen (Tree.Zipper.findNext ((==) (Tree.label prevSibTree)))
                    |> Maybe.map (Tree.Zipper.mapTree (Tree.prependChild selected))
                    |> Maybe.andThen Tree.Zipper.forward
            )
        |> Maybe.withDefault cursor


outdent cursor =
    Tree.Zipper.forward cursor
        |> Maybe.withDefault cursor
