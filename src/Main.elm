port module Main exposing (main)

import BasicsX exposing (..)
import Browser
import Browser.DomX as Dom
import Browser.Events
import HotKey exposing (KeyEvent)
import Html exposing (Html, div, input)
import Html.Attributes exposing (id, tabindex, value)
import Html.Events exposing (onInput)
import Html.Styled
import Html.Styled.Attributes
import Item.Tree
import Item.Zipper exposing (ItemZipper)
import ItemTree exposing (ItemTree)
import Json.Decode exposing (Decoder, decodeValue, errorToString)
import Json.Encode
import List.Extra
import Random exposing (Generator)
import Result.Extra
import Tachyons exposing (classes)
import Tachyons.Classes exposing (..)
import Task
import Toasties
import Toasty
import UI.Tree
import Update
import V exposing (co, cx, t, viewIf)



--port toJsCache : { items : List Item, maybeFocusedItemId : Maybe String } -> Cmd msg


port toJsCache : { cursor : Json.Encode.Value } -> Cmd msg


port toJsPersistToHistory : Json.Encode.Value -> Cmd msg


port toJsUndo : () -> Cmd msg


port toJsRedo : () -> Cmd msg


port toJsError : List String -> Cmd msg


port onJsError : (Err -> msg) -> Sub msg


port onJsLoadFromCouchHistory : (Json.Encode.Value -> msg) -> Sub msg


main =
    Browser.element
        { init = init
        , update = update
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
    , toasties : Toasty.Stack Toasties.Toast
    }


type alias Flags =
    { cache : { cursor : Json.Encode.Value }, now : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    update (Init flags)
        { cursor = ItemTree.initialCursor
        , viewMode = Navigating
        , seed = Random.initialSeed flags.now
        , toasties = Toasty.initialState
        }


overCursorWithHistory : (ItemZipper -> ItemZipper) -> Model -> Model
overCursorWithHistory fn model =
    overCursorWithoutHistory fn model


overCursorWithoutHistory : (ItemZipper -> ItemZipper) -> Model -> Model
overCursorWithoutHistory fn model =
    { model | cursor = fn model.cursor }


setCursor cursor model =
    { model | cursor = cursor }



-- LIB CONFIG


toastyConfig : Toasty.Config msg
toastyConfig =
    Toasties.config



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown <| Json.Decode.map GlobalKeyDown HotKey.keyEventDecoder
        , onJsError OnJsError
        , onJsLoadFromCouchHistory LoadFromCouchHistory
        ]



-- UPDATE


type Msg
    = NOP
    | DomFocusResultReceived (Result String ())
    | GlobalKeyDown KeyEvent
    | Init Flags
    | LoadFromCouchHistory Json.Decode.Value
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
    | ToastyMsg (Toasty.Msg Toasties.Toast)
    | OnJsError Err
    | RotateActionable
    | Undo
    | Redo


fragDomId : String -> String
fragDomId itemId =
    "item-id-" ++ itemId


fragInputDomId : String -> String
fragInputDomId itemId =
    "item-input-dom-id-" ++ itemId


isEditingMode model =
    model.viewMode == EditingSelected


isSelectedBlank model =
    ItemTree.isFragmentBlank model.cursor


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NOP ->
            ( model, Cmd.none )

        ToastyMsg subMsg ->
            Toasty.update toastyConfig ToastyMsg subMsg model

        OnJsError err ->
            Update.pure model
                |> andThenHandleError err

        DomFocusResultReceived (Err msg) ->
            Update.pure model
                |> andThenHandleError ( "DomFocusError", msg )

        DomFocusResultReceived (Ok ()) ->
            ( model, Cmd.none )

        Init flags ->
            loadEncodedCursorAndCache flags.cache.cursor model
                |> Update.andThen ensureFocus

        LoadFromCouchHistory encodedCursor ->
            loadEncodedCursorAndCache encodedCursor model
                |> Update.andThen ensureFocus

        Undo ->
            model
                |> Update.pure
                |> Update.do (toJsUndo ())

        Redo ->
            model
                |> Update.pure
                |> Update.do (toJsRedo ())

        GlobalKeyDown _ ->
            ensureFocus model

        New ->
            if isEditingMode model && isSelectedBlank model then
                --                ( { model | viewMode = Navigating }
                --                    |> overCursorWithHistory ItemTree.deleteIfEmptyAndLeaf
                --                , Cmd.batch []
                --                )
                --                    |> Update.andThen cacheModelAndPersistToHistory
                { model | viewMode = Navigating }
                    |> updateCursorAndCacheWithHistory ItemTree.deleteIfEmptyAndLeaf

            else
                generateId model
                    |> Update.map
                        (\( id, newModel ) ->
                            overCursorWithoutHistory (ItemTree.appendNew id) newModel
                        )
                    |> Update.andThen ensureEditingSelected

        Save ->
            { model | viewMode = Navigating }
                |> updateCursorAndCacheWithHistory ItemTree.deleteIfEmptyAndLeaf

        --            ( { model | viewMode = Navigating }
        --                |> overCursorWithHistory ItemTree.deleteIfEmptyAndLeaf
        --            , Cmd.batch []
        --            )
        --                |> Update.andThen cacheModelAndPersistToHistory
        Delete ->
            ( model |> overCursorWithHistory ItemTree.delete, Cmd.none )
                |> Update.andThen cacheModelAndPersistToHistory

        Edit ->
            ensureEditingSelected model
                |> Update.andThen cacheModelAndPersistToHistory

        CollapseOrPrev ->
            ( model |> overCursorWithHistory ItemTree.collapseOrParent, Cmd.none )
                |> Update.andThen cacheModelAndPersistToHistory

        ExpandOrNext ->
            ( model |> overCursorWithHistory ItemTree.expandOrNext, Cmd.none )
                |> Update.andThen cacheModelAndPersistToHistory

        Prev ->
            Update.pure (overCursorWithoutHistory ItemTree.backward model)
                |> Update.andThen cacheModel

        Next ->
            Update.pure (overCursorWithoutHistory ItemTree.forward model)
                |> Update.andThen cacheModel

        MoveUp ->
            Update.pure (overCursorWithHistory ItemTree.moveUp model)
                |> Update.andThen cacheModelAndPersistToHistory

        MoveDown ->
            Update.pure (overCursorWithHistory ItemTree.moveDown model)
                |> Update.andThen cacheModelAndPersistToHistory

        Outdent ->
            overCursorWithHistory ItemTree.outdent model
                |> Update.pure
                |> Update.andThen cacheModelAndPersistToHistory

        Indent ->
            overCursorWithHistory ItemTree.indent model
                |> Update.pure
                |> Update.andThen cacheModelAndPersistToHistory

        LineChanged newContent ->
            overCursorWithHistory (ItemTree.setContent newContent) model
                |> Update.pure
                |> Update.andThen cacheModelAndPersistToHistory

        RotateActionable ->
            overCursorWithHistory ItemTree.rotateActionable model
                |> Update.pure
                |> Update.andThen cacheModelAndPersistToHistory


type alias Err =
    ( String, String )


addToast : Toasties.Toast -> ModelCmd -> ModelCmd
addToast =
    Toasty.addToast toastyConfig ToastyMsg


addErrorToast : Err -> ModelCmd -> ModelCmd
addErrorToast ( title, detail ) =
    Toasties.Error title detail
        |> addToast


type alias ModelCmd =
    ( Model, Cmd Msg )


andThenHandleError : Err -> ModelCmd -> ModelCmd
andThenHandleError errorTuple =
    let
        sendErrorToJS : Err -> Model -> ModelCmd
        sendErrorToJS ( title, detail ) model =
            ( model, toJsError [ title, detail ] )
    in
    addErrorToast errorTuple
        >> Update.andThen (sendErrorToJS errorTuple)


loadEncodedCursorAndCache encodedCursor model =
    let
        handleCursorDecodeError error =
            ( model, toJsError [ "Cursor Decode Error", errorToString error ] )

        loadCursor cursor =
            Update.pure (setCursor cursor model)
                |> Update.andThen (persistIfChanged OnlyCache model)
    in
    encodedCursor
        |> decodeValue Item.Zipper.decoder
        |> Result.Extra.unpack
            handleCursorDecodeError
            loadCursor


type Persistence
    = OnlyCache
    | CacheAndHistory


updateCursorAndCache fn model =
    overCursorWithoutHistory fn model
        |> persistIfChanged OnlyCache model


updateCursorAndCacheWithHistory fn model =
    overCursorWithoutHistory fn model
        |> persistIfChanged CacheAndHistory model


persistIfChanged persistenceType oldModel newModel =
    if oldModel.cursor /= newModel.cursor then
        let
            encodedCursor =
                Item.Zipper.encoder newModel.cursor

            cacheCmd =
                toJsCache { cursor = encodedCursor }
        in
        ( newModel
        , case persistenceType of
            OnlyCache ->
                cacheCmd

            CacheAndHistory ->
                Cmd.batch [ cacheCmd, toJsPersistToHistory encodedCursor ]
        )

    else
        ( newModel, Cmd.none )


cacheModel model =
    ( model
    , toJsCache <|
        { cursor = Item.Zipper.encoder model.cursor }
    )


persistToHistory model =
    ( model, toJsPersistToHistory <| Item.Zipper.encoder model.cursor )


cacheModelAndPersistToHistory =
    cacheModel >> Update.andThen persistToHistory


generateId model =
    let
        idGen : Generator String
        idGen =
            Random.int 999999999 Random.maxInt
                |> Random.map String.fromInt

        ( randomId, newSeed ) =
            Random.step idGen model.seed

        newModel =
            { model | seed = newSeed }
    in
    Update.pure ( randomId, newModel )


ensureEditingSelected model =
    Update.pure { model | viewMode = EditingSelected }
        |> Update.andThen ensureFocus


ensureFocus model =
    let
        domIdFn =
            case model.viewMode of
                EditingSelected ->
                    fragInputDomId

                Navigating ->
                    fragDomId

        domId =
            domIdFn <| Item.Zipper.id model.cursor
    in
    ( model
    , Dom.focus domId |> Task.attempt DomFocusResultReceived
    )



-- VIEW


fragmentHotKeyDecoder : KeyEvent -> Maybe ( Msg, Bool )
fragmentHotKeyDecoder ke =
    let
        labelKeyMap : List ( KeyEvent -> Bool, ( Msg, Bool ) )
        labelKeyMap =
            [ ( HotKey.is "Enter", New )
            , ( HotKey.is " ", Edit )
            , ( HotKey.isCtrl " ", RotateActionable )
            , ( HotKey.isShift "Enter", NOP )
            , ( HotKey.is "ArrowUp", Prev )
            , ( HotKey.is "ArrowDown", Next )
            , ( HotKey.isMeta "ArrowUp", MoveUp )
            , ( HotKey.isMeta "ArrowDown", MoveDown )
            , ( HotKey.isShift "Tab", Outdent )
            , ( HotKey.is "Tab", Indent )
            , ( HotKey.is "ArrowLeft", CollapseOrPrev )
            , ( HotKey.is "ArrowRight", ExpandOrNext )
            , ( HotKey.is "Delete", Delete )
            , ( HotKey.isMeta "z", Undo )
            , ( HotKey.isKeyMetaShift "z", Redo )
            ]
                |> List.map (overSecond (addSecond True))
    in
    labelKeyMap
        |> List.Extra.find (Tuple.first >> applyTo ke)
        |> Maybe.map Tuple.second


fragmentEditorHotKeyDecoder : KeyEvent -> Maybe ( Msg, Bool )
fragmentEditorHotKeyDecoder ke =
    let
        inputKeyMap : List ( KeyEvent -> Bool, ( Msg, Bool ) )
        inputKeyMap =
            [ ( HotKey.is "Enter", New )
            , ( HotKey.isMeta "Enter", Save )
            , ( HotKey.is "Escape", Save )
            , ( HotKey.isShift "Enter", NOP )
            , ( HotKey.isShift "Tab", Outdent )
            , ( HotKey.is "Tab", Indent )
            ]
                |> List.map (overSecond (addSecond True))
    in
    inputKeyMap
        |> List.Extra.find (Tuple.first >> applyTo ke)
        |> Maybe.map Tuple.second


view : Model -> Html Msg
view model =
    co [ sans_serif, "us-none", ma0, min_vh_100, flex, flex_column ]
        [ viewTreeContainer model
        , Toasty.view toastyConfig Toasties.view ToastyMsg model.toasties
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

        viewPrefix =
            div
                [ cx
                    ([ pr1
                     , flex
                     , items_center
                     , justify_center
                     , lh_solid
                     , code
                     ]
                        ++ additionalStyles
                    )
                ]
                [ t prefix ]
    in
    div [ cx [ flex ] ]
        [ viewPrefix
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
                    [ id <| fragDomId <| Item.Tree.id tree
                    , tabindex 0
                    , HotKey.preventDefaultOnKeyDownEvent fragmentHotKeyDecoder
                    ]
            }


viewFragmentEditor _ tree =
    input
        [ id <| fragInputDomId <| Item.Tree.id tree
        , cx [ ph1, mr1, w_100, bn ]
        , value <| ItemTree.treeFragment tree
        , onInput LineChanged
        , HotKey.preventDefaultOnKeyDownEvent fragmentEditorHotKeyDecoder
        ]
        []
