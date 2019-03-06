port module Main exposing (main)

import BasicsX exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import HotKey exposing (KeyEvent)
import Html exposing (Html, div, input)
import Html.Attributes exposing (tabindex, value)
import Html.Events exposing (onInput)
import Html.Styled
import Html.Styled.Attributes
import Item exposing (Item)
import Item.Zipper exposing (ItemZipper)
import ItemTree exposing (ItemTree)
import Json.Decode exposing (Decoder)
import Json.Encode
import List.Extra
import Maybe.Extra
import Random exposing (Generator)
import Tachyons exposing (classes)
import Tachyons.Classes exposing (..)
import Task
import UI.Tree
import Update
import V exposing (co, cx, t, viewIf)



--port toJsCache : { items : List Item, maybeFocusedItemId : Maybe String } -> Cmd msg


port toJsCache : { cursor : Json.Encode.Value } -> Cmd msg


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
    { cursor : ItemZipper
    , viewMode : ViewMode
    , seed : Random.Seed
    }


type alias Flags =
    { cursor : Json.Encode.Value, now : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    update (Init flags)
        { cursor = ItemTree.initialCursor
        , viewMode = Navigating
        , seed = Random.initialSeed flags.now
        }


overCursor : (ItemZipper -> ItemZipper) -> Model -> Model
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
    | Init Flags
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
    | CollapseOrPrev
    | ExpandOrNext
    | Delete


getItemTreeLabelDomId : ItemTree -> String
getItemTreeLabelDomId tree =
    "item-id-" ++ ItemTree.treeId tree


getItemTreeInputDomId : ItemTree -> String
getItemTreeInputDomId _ =
    {- ++ ItemTree.treeId tree -}
    "item-input-dom-id-"



--cacheNewModel _ =
--    toJsCache { items = [], maybeFocusedItemId = Nothing }


isEditingMode model =
    model.viewMode == EditingSelected


isSelectedBlank model =
    ItemTree.isFragmentBlank model.cursor


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Init flags ->
            let
                cursorResult =
                    Json.Decode.decodeValue Item.Zipper.decoder flags.cursor
                        |> Result.mapError (Json.Decode.errorToString >> Debug.log "Decode Error: Cursor")

                newModel =
                    case cursorResult of
                        Err decodeError ->
                            model

                        Ok cursor ->
                            overCursor (always cursor) model
            in
            Update.pure newModel
                |> Update.andThen ensureFocus

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
                |> Update.andThen cacheModel

        Delete ->
            ( model |> overCursor ItemTree.delete, Cmd.none )

        Edit ->
            ensureEditingSelected model

        CollapseOrPrev ->
            ( model |> overCursor ItemTree.collapseOrParent, Cmd.none )

        ExpandOrNext ->
            ( model |> overCursor ItemTree.expandOrNext, Cmd.none )

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


cacheModel model =
    let
        prettyJson =
            model.cursor
                |> Item.Zipper.encoder
                |> Json.Encode.encode 2
                |> Debug.log "prettyJson"
    in
    ( model
    , toJsCache <|
        { cursor = Item.Zipper.encoder model.cursor }
    )


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


ensureFocus model =
    ( model, ensureFocusCmd model )



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
        , viewTreeContainer model
        ]


viewShortcutHint label shortcut =
    div [ classes [ avenir, ph2, f6, lh_title, br1, ba, b__black_20 ] ]
        [ div [ classes [] ] [ t label ]
        , div [ classes [ light_red ] ] [ t shortcut ]
        ]


viewTreeContainer model =
    let
        root =
            ItemTree.rootTree model.cursor

        isEditing =
            isEditingMode model

        containerClasses =
            let
                baseClasses =
                    [ flex_grow_1, pt3 ]

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
    , cursor : ItemZipper
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
            , ( HotKey.is "ArrowLeft", ( CollapseOrPrev, True ) )
            , ( HotKey.is "ArrowRight", ( ExpandOrNext, True ) )
            , ( HotKey.is "Delete", ( Delete, True ) )
            ]
    in
    labelKeyMap
        |> List.Extra.find (Tuple.first >> applyTo ke)
        |> Maybe.map Tuple.second
        |> Debug.log "itemLabelHotKeyDispatcher"


viewAnyTree vm tree =
    let
        canCollapse =
            ItemTree.canTreeCollapse tree
    in
    div [ classes [] ]
        [ div [ cx [ mb1 ] ] [ viewLine vm tree ]
        , viewIf canCollapse <|
            div [ classes [ pl4 ] ]
                (List.map (viewAnyTree vm) (ItemTree.treeChildren tree))
        ]


fragmentEditorHotKeyDecoder : KeyEvent -> Maybe ( Msg, Bool )
fragmentEditorHotKeyDecoder ke =
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


type ChildrenAre
    = NoChildren
    | Expanded
    | Collapsed


getChildrenState tree =
    let
        canCollapse =
            ItemTree.canTreeCollapse tree

        canExpand =
            ItemTree.canTreeExpand tree
    in
    ter canExpand Expanded (ter canCollapse Collapsed NoChildren)


viewLine vm tree =
    let
        childrenAre =
            getChildrenState tree

        prefix =
            case childrenAre of
                Expanded ->
                    "+"

                Collapsed ->
                    "-"

                NoChildren ->
                    "o"

        additionalStyles =
            if childrenAre == NoChildren || isRootTree tree vm then
                [ o_0 ]

            else
                []
    in
    div [ cx [ flex, inline_flex ] ]
        [ div
            [ cx ([ pr1, flex, items_center, justify_center, lh_solid, code ] ++ additionalStyles)
            ]
            [ t prefix ]
        , if isEditingTree tree vm then
            viewFragmentEditor vm tree

          else
            viewFragment vm tree
        ]


viewFragment vm tree =
    let
        _ =
            1
    in
    Html.Styled.toUnstyled <|
        UI.Tree.viewFragment
            { text = ItemTree.treeFragment tree
            , isRoot = isRootTree tree vm
            , isSelected = isSelectedTree tree vm
            , attrs =
                List.map Html.Styled.Attributes.fromUnstyled
                    [ Html.Attributes.id <| getItemTreeLabelDomId tree
                    , tabindex 0
                    , HotKey.preventDefaultOnKeyDownEvent itemLabelHotKeyDispatcher
                    ]
            }


viewFragmentEditor _ tree =
    input
        [ Html.Attributes.id <| getItemTreeInputDomId tree
        , cx []
        , value <| ItemTree.treeFragment tree
        , onInput LineChanged
        , HotKey.preventDefaultOnKeyDownEvent fragmentEditorHotKeyDecoder
        ]
        []
