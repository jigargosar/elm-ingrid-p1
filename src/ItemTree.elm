module ItemTree exposing
    ( Item
    , ItemCursor
    , ItemTree
    , appendNew
    , backward
    , canTreeCollapse
    , canTreeExpand
    , collapseOrParent
    , delete
    , deleteIfEmptyAndLeaf
    , expandOrNext
    , forward
    , getSelectedTree
    , indent
    , initialCursor
    , isFragmentBlank
    , moveDown
    , moveUp
    , outdent
    , prependNew
    , rootTree
    , setContent
    , treeChildren
    , treeFragment
    , treeId
    )

import BasicsX exposing (..)
import Maybe.Extra
import Tree
import Tree.Zipper as Zipper exposing (Zipper)


orl =
    Maybe.Extra.orElseLazy


type alias Fragment =
    String


type alias Item =
    { id : String
    , fragment : Fragment
    , collapsed : Bool
    }


type alias ItemTree =
    Tree.Tree Item


initialRoot : ItemTree
initialRoot =
    Tree.singleton { id = "ROOT_ITEM_ID", fragment = "Root", collapsed = False }


treeFragment : ItemTree -> Fragment
treeFragment =
    Tree.label >> .fragment


treeId : ItemTree -> Fragment
treeId =
    Tree.label >> .id


createEmptyNode : String -> ItemTree
createEmptyNode id =
    Tree.singleton { id = id, fragment = "", collapsed = False }



--- ITEM Cursor Code ----


type alias ItemCursor =
    Zipper Item


initialCursor : ItemCursor
initialCursor =
    Zipper.fromTree initialRoot


rootTree : ItemCursor -> ItemTree
rootTree =
    Zipper.root >> Zipper.tree


getSelectedTree : ItemCursor -> ItemTree
getSelectedTree =
    Zipper.tree


forward zipper =
    zipper
        |> ifElse (\nz -> canCollapse nz || isLeaf nz)
            Zipper.forward
            (Zipper.nextSibling
                >> orl (\_ -> nextSiblingOfParent zipper)
            )
        |> Maybe.withDefault zipper


nextSiblingOfParent : ItemCursor -> Maybe ItemCursor
nextSiblingOfParent zipper =
    if isRoot zipper then
        Nothing

    else
        zipper
            |> Zipper.parent
            |> Maybe.andThen
                (\nz ->
                    Zipper.nextSibling nz
                        |> orl (\_ -> nextSiblingOfParent nz)
                )


backward zipper =
    zipper
        |> lastVisibleDescendentOfPrevSib
        |> orl (\_ -> Zipper.parent zipper)
        |> Maybe.withDefault zipper


lastVisibleDescendentOfPrevSib zipper =
    zipper
        |> Zipper.previousSibling
        |> Maybe.map lastVisibleDescendentOrSelf


lastVisibleDescendentOrSelf z =
    if canCollapse z then
        z
            |> Zipper.lastChild
            |> Maybe.map lastVisibleDescendentOrSelf
            |> Maybe.withDefault z

    else
        z


appendNew : String -> ItemCursor -> ItemCursor
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


prependNew : String -> ItemCursor -> ItemCursor
prependNew id zipper =
    if isRoot zipper then
        appendNew id zipper

    else
        let
            newTree =
                createEmptyNode id
        in
        Zipper.prepend newTree zipper
            |> Zipper.findPrevious (eqs (Tree.label newTree))
            |> Maybe.withDefault zipper


treeChildren : ItemTree -> List ItemTree
treeChildren tree =
    Tree.children tree


indent zipper =
    let
        selectedT =
            Zipper.tree zipper
    in
    Zipper.previousSibling zipper
        |> Maybe.map (Zipper.tree >> Tree.label)
        |> Maybe.andThen
            (\prevSibLabel ->
                Zipper.removeTree zipper
                    |> Maybe.andThen (Zipper.findNext (eqs prevSibLabel))
                    |> Maybe.map (Zipper.mapTree (Tree.appendChild selectedT) >> expand)
                    |> Maybe.andThen Zipper.lastChild
            )
        |> Maybe.withDefault zipper


outdent zipper =
    let
        selectedT =
            Zipper.tree zipper
    in
    if Zipper.parent zipper == (Just <| Zipper.root zipper) then
        zipper

    else
        Zipper.removeTree zipper
            |> Maybe.map (Zipper.append selectedT)
            |> Maybe.andThen Zipper.nextSibling
            |> Maybe.withDefault zipper


moveUp zipper =
    let
        selectedT =
            Zipper.tree zipper
    in
    Zipper.previousSibling zipper
        |> Maybe.map (Zipper.tree >> Tree.label)
        |> Maybe.andThen
            (\prevSibLabel ->
                Zipper.removeTree zipper
                    |> Maybe.andThen (Zipper.findNext (eqs prevSibLabel))
                    |> Maybe.map (Zipper.prepend selectedT)
                    |> Maybe.andThen Zipper.previousSibling
            )
        |> Maybe.withDefault zipper


moveDown zipper =
    let
        selectedT =
            Zipper.tree zipper
    in
    Zipper.nextSibling zipper
        |> Maybe.map (Zipper.tree >> Tree.label)
        |> Maybe.andThen
            (\nextSibLabel ->
                Zipper.removeTree zipper
                    |> Maybe.andThen (Zipper.findNext (eqs nextSibLabel))
                    |> Maybe.map (Zipper.append selectedT)
                    |> Maybe.andThen Zipper.nextSibling
            )
        |> Maybe.withDefault zipper


setContent newContent =
    Zipper.mapLabel (\label -> { label | fragment = newContent })


isRoot zipper =
    zipper == Zipper.root zipper


isNotRoot =
    isRoot >> not


getFragment : ItemCursor -> String
getFragment =
    Zipper.label >> .fragment


isFragmentBlank : ItemCursor -> Bool
isFragmentBlank =
    getFragment >> String.trim >> String.isEmpty


isLeaf =
    Zipper.children >> List.length >> eqs 0


deleteIfEmptyAndLeaf zipper =
    if isLeaf zipper && isFragmentBlank zipper then
        delete zipper

    else
        zipper


delete zipper =
    let
        maybeNext : Maybe Item
        maybeNext =
            Zipper.nextSibling zipper
                |> orl (\_ -> Zipper.backward zipper)
                |> Maybe.map Zipper.label
    in
    maybeNext
        |> Debug.log "maybeNext"
        |> Maybe.andThen
            (\next ->
                Zipper.removeTree zipper
                    |> Maybe.andThen
                        (\newZipper ->
                            Zipper.findNext (eqs next) newZipper
                                |> orl (\_ -> Zipper.findPrevious (eqs next) newZipper)
                                |> orl (\_ -> Zipper.findFromRoot (eqs next) newZipper)
                        )
            )
        |> Maybe.withDefault zipper


collapse zipper =
    if isRoot zipper then
        zipper

    else
        Zipper.mapLabel (\item -> { item | collapsed = True }) zipper


parentOrSelf z =
    Zipper.parent z |> Maybe.withDefault z


collapseOrParent =
    ifElse canCollapse collapse parentOrSelf


expand =
    Zipper.mapLabel (\item -> { item | collapsed = False })


expandOrNext =
    ifElse canExpand expand forward


canTreeCollapse tree =
    let
        item =
            Tree.label tree
    in
    treeHasChildren tree && not item.collapsed


canTreeExpand tree =
    let
        item =
            Tree.label tree
    in
    treeHasChildren tree && item.collapsed


treeHasChildren =
    Tree.children >> List.head >> Maybe.Extra.isJust


isTreeLeaf =
    treeHasChildren >> not


canCollapse =
    Zipper.tree >> canTreeCollapse


canExpand =
    Zipper.tree >> canTreeExpand
