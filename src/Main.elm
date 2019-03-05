port module Main exposing (main)

import BasicsX exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import HotKey exposing (KeyEvent)
import Html exposing (Html, div, input, textarea)
import Html.Attributes exposing (style, tabindex, value)
import Html.Events exposing (onInput)
import ItemTree exposing (Item, ItemTree, ItemTreeCursor)
import Json.Decode exposing (Decoder)
import List.Extra
import Maybe.Extra
import Random exposing (Generator)
import Tachyons exposing (classes)
import Tachyons.Classes exposing (..)
import Task
import TreeView
import Update
import V exposing (co, cx, t)


port toJsCache : { items : List Item, maybeFocusedItemId : Maybe String } -> Cmd msg


main =
    Browser.element
        { init = init
        , update =
            \msg -> update (Debug.log "msg" msg)
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type ViewMode
    = Navigating
    | EditingSelected


type alias Model =
    { maybeFocusedItemId : Maybe String
    , cursor : ItemTreeCursor
    , viewMode : ViewMode
    , seed : Random.Seed
    }


type alias Flags =
    { items : List Item, maybeFocusedItemId : Maybe String, now : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    update InitReceived
        { maybeFocusedItemId = flags.maybeFocusedItemId
        , cursor = ItemTree.initialCursor
        , viewMode = Navigating
        , seed = Random.initialSeed flags.now
        }


overCursor : (ItemTreeCursor -> ItemTreeCursor) -> Model -> Model
overCursor fn model =
    { model | cursor = fn model.cursor }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown <| Json.Decode.map GlobalKeyDown HotKey.keyEventDecoder
        ]



-- UPDATE


type Msg
    = NOP
    | GlobalKeyDown KeyEvent
    | InitReceived
    | LineChanged String
    | New
    | Save
    | Prev
    | Edit
    | Next
    | MoveUp
    | MoveDown
    | Outdent
    | Indent
    | Collapse
    | Expand
    | Delete


getItemTreeLabelDomId : ItemTree -> String
getItemTreeLabelDomId tree =
    "item-id-" ++ ItemTree.treeId tree


getItemTreeInputDomId : ItemTree -> String
getItemTreeInputDomId tree =
    {- ++ ItemTree.treeId tree -}
    "item-input-dom-id-"


cacheNewModel _ =
    toJsCache { items = [], maybeFocusedItemId = Nothing }


isEditingMode model =
    model.viewMode == EditingSelected


isSelectedBlank model =
    ItemTree.isFragmentBlank model.cursor


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        InitReceived ->
            ( model, ensureFocusCmd model )

        GlobalKeyDown _ ->
            ( model, ensureFocusCmd model )

        NOP ->
            ( model, Cmd.none )

        New ->
            if isEditingMode model && isSelectedBlank model then
                stopEditing model

            else
                generateId model
                    |> Update.map
                        (\( id, newModel ) ->
                            overCursor (ItemTree.appendNew id) newModel
                        )
                    |> Update.andThen ensureEditingSelected

        Save ->
            stopEditing model

        Delete ->
            ( model |> overCursor ItemTree.delete, Cmd.none )

        Edit ->
            ensureEditingSelected model

        Collapse ->
            ( model |> overCursor ItemTree.collapse, Cmd.none )

        Expand ->
            ( model |> overCursor ItemTree.expand, Cmd.none )

        Prev ->
            Update.pure (overCursor ItemTree.backward model)
                |> Update.effect ensureFocusCmd

        Next ->
            Update.pure (overCursor ItemTree.forward model)
                |> Update.effect ensureFocusCmd

        MoveUp ->
            Update.pure (overCursor ItemTree.moveUp model)
                |> Update.effect ensureFocusCmd

        MoveDown ->
            Update.pure (overCursor ItemTree.moveDown model)
                |> Update.effect ensureFocusCmd

        Outdent ->
            ( overCursor ItemTree.outdent model
            , ensureFocusCmd model
            )

        Indent ->
            ( overCursor ItemTree.indent model
            , ensureFocusCmd model
            )

        LineChanged newContent ->
            ( overCursor (ItemTree.setContent newContent) model, Cmd.none )


stopEditing model =
    ( { model | viewMode = Navigating }
        |> overCursor ItemTree.deleteIfEmptyAndLeaf
    , Cmd.batch []
    )


generateId model =
    let
        idGen : Generator String
        idGen =
            Random.int 999999999 Random.maxInt
                |> Random.map String.fromInt

        ( id, newSeed ) =
            Random.step idGen model.seed

        newModel =
            { model | seed = newSeed }
    in
    Update.pure ( id, newModel )


ensureEditingSelected model =
    if model.viewMode == EditingSelected then
        ( model, Cmd.none )

    else
        ( { model | viewMode = EditingSelected }
        , Cmd.batch [ focusInputCmd model ]
        )


focusInputCmd model =
    Browser.Dom.focus (getItemTreeInputDomId <| ItemTree.getSelectedTree model.cursor)
        |> Task.attempt (Debug.log "focusing master input" >> Debug.log "focusInputCmd" >> (\_ -> NOP))


focusSelectedCmd model =
    Browser.Dom.focus (getItemTreeLabelDomId <| ItemTree.getSelectedTree model.cursor)
        |> Task.attempt (Debug.log "focusing selected label" >> Debug.log "focusSelectedCmd" >> (\_ -> NOP))


ensureFocusCmd model =
    if model.viewMode == EditingSelected then
        Cmd.batch [ focusInputCmd model ]

    else
        Cmd.batch [ focusSelectedCmd model ]



-- VIEW


view : Model -> Html Msg
view model =
    co [ sans_serif, "us-none", ma0, min_vh_100, flex, flex_column ]
        [ div [ classes [ flex, bg_black, white ] ]
            [ viewShortcutHint "Line Below" "Enter"
            , viewShortcutHint "Line Above" "Shift+Enter"
            , viewShortcutHint "Indent" "Tab"
            , viewShortcutHint "Outdent" "Shift+Tab"
            , viewShortcutHint "Move Up" "Cmd+Up"
            , viewShortcutHint "Move Down" "Cmd+Down"
            ]

        --        , viewTreeContainer model
        , viewAnyTreeContainer model
        ]


viewShortcutHint label shortcut =
    div [ classes [ avenir, ph2, f6, lh_title, br1, ba, b__black_20 ] ]
        [ div [ classes [] ] [ t label ]
        , div [ classes [ light_red ] ] [ t shortcut ]
        ]


viewAnyTreeContainer model =
    let
        root =
            ItemTree.rootTree model.cursor

        isEditing =
            isEditingMode model

        containerClasses =
            let
                baseClasses =
                    [ flex_grow_1, pl3 ]

                editingModeClasses =
                    [ bg_black_20, black_50 ]
            in
            baseClasses |> concatIf isEditing editingModeClasses
    in
    div [ classes containerClasses ]
        [ viewAnyTree { isEditingMode = isEditing, cursor = model.cursor } root
        ]


type alias TreeViewModel =
    { isEditingMode : Bool
    , cursor : ItemTreeCursor
    }


isSelectedTree : ItemTree -> TreeViewModel -> Bool
isSelectedTree tree treeVM =
    ItemTree.getSelectedTree treeVM.cursor == tree


isEditingTree : ItemTree -> TreeViewModel -> Bool
isEditingTree tree treeVM =
    treeVM.isEditingMode && isSelectedTree tree treeVM


isRootTree : ItemTree -> TreeViewModel -> Bool
isRootTree tree treeVM =
    ItemTree.rootTree treeVM.cursor == tree


itemLabelHotKeyDispatcher : KeyEvent -> Maybe ( Msg, Bool )
itemLabelHotKeyDispatcher ke =
    let
        labelKeyMap : List ( KeyEvent -> Bool, ( Msg, Bool ) )
        labelKeyMap =
            [ ( HotKey.is "Enter", ( New, True ) )
            , ( HotKey.is " ", ( Edit, True ) )
            , ( HotKey.isShift "Enter", ( NOP, True ) )
            , ( HotKey.is "ArrowUp", ( Prev, True ) )
            , ( HotKey.is "ArrowDown", ( Next, True ) )
            , ( HotKey.isMeta "ArrowUp", ( MoveUp, True ) )
            , ( HotKey.isMeta "ArrowDown", ( MoveDown, True ) )
            , ( HotKey.isShift "Tab", ( Outdent, True ) )
            , ( HotKey.is "Tab", ( Indent, True ) )
            , ( HotKey.is "ArrowLeft", ( Collapse, True ) )
            , ( HotKey.is "ArrowRight", ( Expand, True ) )
            , ( HotKey.is "Delete", ( Delete, True ) )
            ]
    in
    labelKeyMap
        |> List.Extra.find (Tuple.first >> applyTo ke)
        |> Maybe.map Tuple.second
        |> Debug.log "itemLabelHotKeyDispatcher"


viewAnyTree treeVM tree =
    let
        sel =
            isSelectedTree tree treeVM
    in
    div [ classes [] ]
        [ if isEditingTree tree treeVM then
            viewFragmentEditor treeVM tree

          else
            TreeView.viewFragment
                { text = ItemTree.treeFragment tree |> (++) (ter (ItemTree.isTreeExpanded tree) "-" "+")
                , isRoot = isRootTree tree treeVM
                , isSelected = sel
                , attrs =
                    [ Html.Attributes.id <| getItemTreeLabelDomId tree
                    , tabindex 0
                    , HotKey.preventDefaultOnKeyDownEvent itemLabelHotKeyDispatcher
                    ]
                }
        , div [ classes [ pl3, pt2 ] ]
            (List.map (viewAnyTree treeVM) (ItemTree.treeChildren tree))
        ]


itemEditorHotKeyDispatcher : KeyEvent -> Maybe ( Msg, Bool )
itemEditorHotKeyDispatcher ke =
    let
        inputKeyMap : List ( KeyEvent -> Bool, ( Msg, Bool ) )
        inputKeyMap =
            [ ( HotKey.is "Enter", ( New, True ) )
            , ( HotKey.isMeta "Enter", ( Save, True ) )
            , ( HotKey.is "Escape", ( Save, True ) )
            , ( HotKey.isShift "Enter", ( NOP, True ) )
            , ( HotKey.isShift "Tab", ( Outdent, True ) )
            , ( HotKey.is "Tab", ( Indent, True ) )
            ]
    in
    inputKeyMap
        |> List.Extra.find (Tuple.first >> applyTo ke)
        |> Maybe.map Tuple.second
        |> Maybe.Extra.orElse (Just ( NOP, False ))
        |> Debug.log "itemEditorHotKeyDispatcher"


viewFragmentEditor treeVM tree =
    input
        [ Html.Attributes.id <| getItemTreeInputDomId tree
        , cx []
        , value <| ItemTree.treeFragment tree
        , onInput LineChanged
        , HotKey.preventDefaultOnKeyDownEvent itemEditorHotKeyDispatcher
        ]
        []



--viewTreeContainer model =
--    let
--        root =
--            ItemTree.rootTree model.cursor
--
--        selected =
--            ItemTree.getSelectedTree model.cursor
--
--        isEditing =
--            model.viewMode == EditingSelected
--
--        containerClasses =
--            let
--                baseClasses =
--                    [ flex_grow_1, pl3 ]
--
--                editingModeClasses =
--                    [ bg_black_20, black_50 ]
--            in
--            if isEditing then
--                baseClasses ++ editingModeClasses
--
--            else
--                baseClasses
--    in
--    div [ classes containerClasses ]
--        [ viewRootItemTreeLabel isEditing selected root
--        , viewItemForest isEditing selected (ItemTree.treeChildren root)
--        ]
--
--
--viewItemForest : Bool -> ItemTree -> List ItemTree -> Html Msg
--viewItemForest isEditingMode selected forest =
--    div [ classes [ pl3 ] ]
--        (forest
--            |> List.map (viewItemTree isEditingMode selected)
--        )
--
--
--viewItemTree isEditingMode selected tree =
--    div []
--        [ if isEditingMode && selected == tree then
--            viewEditItemLabel tree
--
--          else
--            viewItemTreeLabel selected tree
--        , viewItemForest isEditingMode selected (ItemTree.treeChildren tree)
--        ]
--
--
--viewItemTreeLabel selected tree =
--    let
--        labelClasses =
--            let
--                defaultClasses =
--                    [ pa1, dib, br1 ]
--
--                selectedClasses =
--                    [ bg_light_red, white ]
--
--                notSelectedClasses =
--                    []
--            in
--            if tree == selected then
--                defaultClasses ++ selectedClasses
--
--            else
--                defaultClasses ++ notSelectedClasses
--    in
--    div [ classes [ h2, flex, items_center ] ]
--        [ div [ classes labelClasses ]
--            [ t <| ItemTree.treeFragment tree, t " ", t <| ItemTree.treeId tree ]
--        ]
--
--
--viewRootItemTreeLabel isEditing selected rootTree =
--    let
--        labelClasses =
--            let
--                defaultClasses =
--                    [ pa1, dib, f3, br1 ]
--
--                selectedClasses =
--                    [ bg_light_red, white ]
--            in
--            if rootTree == selected then
--                defaultClasses ++ selectedClasses
--
--            else
--                defaultClasses
--    in
--    div [ classes [ pv2 ] ]
--        [ div [ classes labelClasses ]
--            [ t <| ItemTree.treeFragment rootTree ]
--        ]
--
--
--
--viewEditItemLabel tree =
--    let
--        content =
--            ItemTree.treeFragment tree
--    in
--    div [ classes [ dib, mv1, pa2, br1, bg_white ] ]
--        [ div [ classes [ dib, relative, "pre-wrap", "break-word" ], style "min-width" "10rem" ]
--            [ textarea
--                [ Html.Attributes.id (getItemTreeInputDomId tree)
--                , classes
--                    [ pa0
--                    , bn
--                    , absolute
--                    , "resize-none"
--
--                    --                    , o_50
--                    , w_100
--                    , h_100
--                    , outline_0
--                    , "pre-wrap"
--                    , "break-word"
--                    ]
--                , value content
--                , onInput LineChanged
--                , HotKey.preventDefaultOnKeyDownEvent itemEditorHotKeyDispatcher
--                ]
--                []
--            , div [ classes [ dib ], style "min-width" "10rem" ] [ t content ]
--            ]
--        ]
