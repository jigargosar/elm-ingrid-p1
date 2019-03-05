port module Main exposing (main)

import BasicsX exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import HotKey exposing (KeyEvent)
import Html exposing (Html, div, textarea)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput)
import ItemTree exposing (Item, ItemTree, ItemTreeCursor)
import Json.Decode exposing (Decoder)
import List.Extra
import Maybe.Extra
import Random exposing (Generator)
import Tachyons exposing (classes)
import Tachyons.Classes exposing (..)
import Task
import Update
import V exposing (co, t)


port toJsCache : { items : List Item, maybeFocusedItemId : Maybe String } -> Cmd msg


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



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
        [ Browser.Events.onKeyDown <| Json.Decode.map KeyDownReceived HotKey.keyEventDecoder
        ]



-- UPDATE


type Msg
    = NOP
    | KeyDownReceived KeyEvent
    | InitReceived
    | ContentChanged String
    | InputKeyEventReceived KeyEvent
    | NewLine
    | SaveLine


getItemDomId : Item -> String
getItemDomId item =
    "item-id-" ++ item.id


getItemInputDomId : ItemTree -> String
getItemInputDomId tree =
    {- ++ ItemTree.treeId tree -}
    "item-input-dom-id-"


cacheNewModel _ =
    toJsCache { items = [], maybeFocusedItemId = Nothing }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NOP ->
            ( model, Cmd.none )

        NewLine ->
            newLine model

        SaveLine ->
            saveEditingLine model

        ContentChanged newContent ->
            ( overCursor (ItemTree.setContent newContent) model, Cmd.none )

        InitReceived ->
            ( model, Cmd.none )

        InputKeyEventReceived keyEvent ->
            let
                _ =
                    Debug.log "InputKeyEventReceived" keyEvent
            in
            ( model
            , Cmd.batch
                []
            )

        KeyDownReceived keyEvent ->
            let
                {- _ =
                   Debug.log "KeyDownReceived" keyEvent
                -}
                keyMap =
                    case model.viewMode of
                        Navigating ->
                            navKeyMap

                        EditingSelected ->
                            []

                findMapping =
                    List.Extra.find (Tuple.first >> applyTo keyEvent)
            in
            keyMap
                |> findMapping
                |> Maybe.Extra.orElseLazy (\_ -> findMapping globalKeyMap)
                |> Maybe.map (\( _, mFn ) -> mFn model)
                |> Maybe.withDefault ( model, Cmd.none )


globalKeyMap : List ( KeyEvent -> Bool, Model -> ( Model, Cmd Msg ) )
globalKeyMap =
    [ ( HotKey.isShift "Tab", outdent )
    , ( HotKey.is "Tab", indent )
    ]


navKeyMap : List ( KeyEvent -> Bool, Model -> ( Model, Cmd Msg ) )
navKeyMap =
    [ ( HotKey.is "Enter", newLine )
    , ( HotKey.is " ", edit )
    , ( HotKey.isShift "Enter", prependNewAndStartEditing )
    , ( HotKey.is "ArrowUp", selectPrev )
    , ( HotKey.is "ArrowDown", selectNext )
    , ( HotKey.isMeta "ArrowUp", moveUp )
    , ( HotKey.isMeta "ArrowDown", moveDown )
    ]


saveEditingLine model =
    ( { model | viewMode = Navigating }
    , Cmd.batch []
    )


focusInputCmd model =
    Browser.Dom.focus (getItemInputDomId <| ItemTree.getSelectedTree model.cursor)
        |> Task.attempt (Debug.log "focusing master input" >> Debug.log "focusInputCmd" >> (\_ -> NOP))


initEditingMode model =
    ( { model | viewMode = EditingSelected }
    , Cmd.batch [ focusInputCmd model ]
    )


ensureEditInputFocusCmd model =
    if model.viewMode == EditingSelected then
        Cmd.batch [ focusInputCmd model ]

    else
        Cmd.batch []


edit model =
    if model.viewMode == EditingSelected then
        ( model
        , Cmd.batch []
        )

    else
        initEditingMode model


moveUp model =
    Update.pure (overCursor ItemTree.moveUp model)
        |> Update.effect ensureEditInputFocusCmd


moveDown model =
    Update.pure (overCursor ItemTree.moveDown model)
        |> Update.effect ensureEditInputFocusCmd


indent model =
    ( overCursor ItemTree.indent model
    , ensureEditInputFocusCmd model
    )


outdent model =
    ( overCursor ItemTree.outdent model
    , ensureEditInputFocusCmd model
    )


selectPrev model =
    Update.pure (overCursor ItemTree.backward model)
        |> Update.effect ensureEditInputFocusCmd


selectNext model =
    Update.pure (overCursor ItemTree.forward model)
        |> Update.effect ensureEditInputFocusCmd


withNewId fn model =
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
    fn id newModel


newLine : Model -> ( Model, Cmd Msg )
newLine =
    withNewId
        (\id model ->
            Update.pure
                { model
                    | cursor = ItemTree.appendNew id model.cursor
                }
                |> Update.andThen initEditingMode
        )


prependNewAndStartEditing : Model -> ( Model, Cmd Msg )
prependNewAndStartEditing =
    withNewId
        (\id model ->
            Update.pure
                { model
                    | cursor = ItemTree.prependNew id model.cursor
                }
                |> Update.andThen initEditingMode
        )



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

        selected =
            ItemTree.getSelectedTree model.cursor

        isEditing =
            model.viewMode == EditingSelected

        containerClasses =
            let
                baseClasses =
                    [ flex_grow_1, pl3 ]

                editingModeClasses =
                    [ bg_black_20, black_50 ]
            in
            if isEditing then
                baseClasses ++ editingModeClasses

            else
                baseClasses
    in
    div [ classes containerClasses ]
        [ viewAnyTree { isEditingMode = isEditing, cursor = model.cursor } root
        ]


type alias TreeViewModel =
    { isEditingMode : Bool
    , cursor : ItemTreeCursor
    }


isEditingTree : ItemTree -> TreeViewModel -> Bool
isEditingTree tree treeVM =
    treeVM.isEditingMode && ItemTree.getSelectedTree treeVM.cursor == tree


isRootTree : ItemTree -> TreeViewModel -> Bool
isRootTree tree treeVM =
    ItemTree.rootTree treeVM.cursor == tree


viewAnyTree treeVM tree =
    div [ classes [] ]
        [ if isEditingTree tree treeVM then
            viewAnyTreeEditLabel treeVM tree

          else
            viewAnyTreeDisplayLabel treeVM tree
        , div [ classes [ pl3 ] ]
            (List.map (viewAnyTree treeVM) (ItemTree.treeChildren tree))
        ]


viewAnyTreeEditLabel treeVM tree =
    div [ classes [] ] []


viewAnyTreeDisplayLabel treeVM tree =
    div [ classes [] ] []


viewTreeContainer model =
    let
        root =
            ItemTree.rootTree model.cursor

        selected =
            ItemTree.getSelectedTree model.cursor

        isEditing =
            model.viewMode == EditingSelected

        containerClasses =
            let
                baseClasses =
                    [ flex_grow_1, pl3 ]

                editingModeClasses =
                    [ bg_black_20, black_50 ]
            in
            if isEditing then
                baseClasses ++ editingModeClasses

            else
                baseClasses
    in
    div [ classes containerClasses ]
        [ viewRootItemTreeLabel isEditing selected root
        , viewItemForest isEditing selected (ItemTree.treeChildren root)
        ]


viewItemForest : Bool -> ItemTree -> List ItemTree -> Html Msg
viewItemForest isEditingMode selected forest =
    div [ classes [ pl3 ] ]
        (forest
            |> List.map (viewItemTree isEditingMode selected)
        )


viewItemTree isEditingMode selected tree =
    div []
        [ if isEditingMode && selected == tree then
            viewEditItemLabel tree

          else
            viewItemTreeLabel selected tree
        , viewItemForest isEditingMode selected (ItemTree.treeChildren tree)
        ]


viewItemTreeLabel selected tree =
    let
        labelClasses =
            let
                defaultClasses =
                    [ pa1, dib, br1 ]

                selectedClasses =
                    [ bg_light_red, white ]

                notSelectedClasses =
                    []
            in
            if tree == selected then
                defaultClasses ++ selectedClasses

            else
                defaultClasses ++ notSelectedClasses
    in
    div [ classes [ h2, flex, items_center ] ]
        [ div [ classes labelClasses ]
            [ t <| ItemTree.treeFragment tree, t " ", t <| ItemTree.treeId tree ]
        ]


viewRootItemTreeLabel isEditing selected rootTree =
    let
        labelClasses =
            let
                defaultClasses =
                    [ pa1, dib, f3, br1 ]

                selectedClasses =
                    [ bg_light_red, white ]
            in
            if rootTree == selected then
                defaultClasses ++ selectedClasses

            else
                defaultClasses
    in
    div [ classes [ pv2 ] ]
        [ div [ classes labelClasses ]
            [ t <| ItemTree.treeFragment rootTree ]
        ]


itemEditorHotKeyDispatcher : KeyEvent -> Maybe ( Msg, Bool )
itemEditorHotKeyDispatcher ke =
    let
        inputKeyMap : List ( KeyEvent -> Bool, ( Msg, Bool ) )
        inputKeyMap =
            [ ( HotKey.is "Enter", ( NewLine, True ) )
            , ( HotKey.isMeta "Enter", ( SaveLine, True ) )
            , ( HotKey.is "Escape", ( SaveLine, True ) )
            , ( HotKey.isShift "Enter", ( NOP, True ) )
            ]
    in
    inputKeyMap
        |> List.Extra.find (Tuple.first >> applyTo ke)
        |> Maybe.map Tuple.second
        |> Debug.log "itemEditorHotKeyDispatcher"


viewEditItemLabel tree =
    let
        content =
            ItemTree.treeFragment tree
    in
    div [ classes [ dib, mv1, pa2, br1, bg_white ] ]
        [ div [ classes [ dib, relative, "pre-wrap", "break-word" ], style "min-width" "10rem" ]
            [ textarea
                [ Html.Attributes.id (getItemInputDomId tree)
                , classes
                    [ pa0
                    , bn
                    , absolute
                    , "resize-none"

                    --                    , o_50
                    , w_100
                    , h_100
                    , outline_0
                    , "pre-wrap"
                    , "break-word"
                    ]
                , value content
                , onInput ContentChanged
                , HotKey.preventDefaultOnKeyDownEvent itemEditorHotKeyDispatcher
                ]
                []
            , div [ classes [ dib ], style "min-width" "10rem" ] [ t content ]
            ]
        ]
