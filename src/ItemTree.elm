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

import List.Extra
import Tree
import Tree.Zipper as Zipper exposing (Zipper)


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
    Zipper Item


initialRoot : ItemTree
initialRoot =
    Tree.singleton { id = "ROOT_ITEM_ID", fragment = "Root", collapsed = False }


initialCursor : ItemTreeCursor
initialCursor =
    Zipper.fromTree initialRoot


rootTree : ItemTreeCursor -> ItemTree
rootTree =
    Zipper.root >> Zipper.tree


treeFragment : ItemTree -> Fragment
treeFragment =
    Tree.label >> .fragment


treeId : ItemTree -> Fragment
treeId =
    Tree.label >> .id


selectedTree : ItemTreeCursor -> ItemTree
selectedTree =
    Zipper.tree


appendNew : String -> ItemTreeCursor -> ItemTreeCursor
appendNew id zipper =
    let
        newTree =
            createEmptyNode id

        newZipper =
            if zipper == Zipper.root zipper then
                Zipper.mapTree (Tree.prependChild newTree) zipper

            else
                Zipper.append newTree zipper
    in
    Zipper.findNext (eqs (Tree.label newTree)) newZipper
        |> Maybe.withDefault newZipper


prependNew : String -> ItemTreeCursor -> ItemTreeCursor
prependNew id zipper =
    if zipper == Zipper.root zipper then
        appendNew id zipper

    else
        let
            newTree =
                createEmptyNode id
        in
        Zipper.prepend newTree zipper
            |> Zipper.findPrevious (eqs (Tree.label newTree))
            |> Maybe.withDefault zipper


createEmptyNode : String -> ItemTree
createEmptyNode id =
    Tree.singleton { id = id, fragment = "Empty", collapsed = False }


treeChildren : ItemTree -> List ItemTree
treeChildren tree =
    Tree.children tree


backward cursor =
    Zipper.backward cursor
        |> Maybe.withDefault cursor


forward cursor =
    Zipper.forward cursor
        |> Maybe.withDefault cursor


eqs =
    (==)


indent cursor =
    let
        selected =
            selectedTree cursor
    in
    Zipper.previousSibling cursor
        |> Maybe.map (Zipper.tree >> Tree.label)
        |> Maybe.andThen
            (\prevSibLabel ->
                Zipper.removeTree cursor
                    |> Maybe.andThen (Zipper.findNext (eqs prevSibLabel))
                    |> Maybe.map (Zipper.mapTree (Tree.appendChild selected))
                    |> Maybe.andThen Zipper.lastChild
            )
        |> Maybe.withDefault cursor


outdent cursor =
    Zipper.forward cursor
        |> Maybe.withDefault cursor
