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


type alias Fragment =
    String


type alias NodeModel =
    { fragment : Fragment
    , collapsed : Bool
    , children : Array ItemTreeNode
    }


type ItemTreeNode
    = Node NodeModel


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


selectedNode : ItemTreeCursor -> ItemTreeNode
selectedNode cursor =
    cursor.root


isSelected : ItemTreeNode -> ItemTreeCursor -> Bool
isSelected node cursor =
    True


updateNode : (NodeModel -> NodeModel) -> ItemTreeNode -> ItemTreeNode
updateNode fn (Node nodeModel) =
    fn nodeModel |> Node


updateSelectedNode fn cursor =
    let
        oldNode =
            selectedNode cursor

        newNode =
            updateNode fn oldNode
    in
    cursor


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


appendEmptyFragment cursor =
    let
        root =
            cursor.root
    in
    if selectedNode cursor == currentRoot cursor then
        prependChild cursor

    else
        appendSibling cursor


createEmptyNode : ItemTreeNode
createEmptyNode =
    Node { fragment = "Empty", collapsed = False, children = Array.empty }
