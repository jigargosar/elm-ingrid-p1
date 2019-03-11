port module Main exposing (main)

import BasicsX exposing (..)
import Browser
import Browser.DomX as Dom
import Browser.Events
import CommandMode as CM exposing (CommandModeMsg)
import EditMode as EM exposing (EditModeMsg)
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
import V exposing (attrIf, co, cx, t, viewIf)


port toJs : Json.Encode.Value -> Cmd msg


port fromJs : (Json.Encode.Value -> msg) -> Sub msg


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


type EditType
    = E_New
    | E_Existing


type EditorMode
    = CommandMode
    | EditSelected EditType


type alias Model =
    { cursor : ItemZipper
    , editorMode : EditorMode
    , seed : Random.Seed
    , toasties : Toasty.Stack Toasties.Toast
    }


type alias Flags =
    { cache : { cursor : Json.Encode.Value }, now : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    update (Init flags)
        { cursor = ItemTree.initialCursor
        , editorMode = CommandMode
        , seed = Random.initialSeed flags.now
        , toasties = Toasty.initialState
        }


overCursor : (ItemZipper -> ItemZipper) -> Model -> Model
overCursor fn model =
    { model | cursor = fn model.cursor }


setCursor : ItemZipper -> Model -> Model
setCursor =
    always >> overCursor


setEditingNew model =
    { model | editorMode = EditSelected E_New }



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
    | ToastyMsg (Toasty.Msg Toasties.Toast)
    | OnJsError Err
    | EMMsgReceived EditModeMsg
    | CMMsgReceived CommandModeMsg


fragDomId : String -> String
fragDomId itemId =
    "item-id-" ++ itemId


fragInputDomId : String -> String
fragInputDomId itemId =
    "item-input-dom-id-" ++ itemId


isEditingSelected : Model -> Bool
isEditingSelected model =
    case model.editorMode of
        EditSelected _ ->
            True

        CommandMode ->
            False


isEditingNew : Model -> Bool
isEditingNew model =
    model.editorMode == EditSelected E_New


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

        GlobalKeyDown _ ->
            ensureFocus model

        CMMsgReceived msg ->
            handleCommandMsg msg model

        EMMsgReceived msg ->
            handleEditMsg msg model


handleCommandMsg msg model =
    case msg of
        CM.Undo ->
            model
                |> Update.pure
                |> Update.do (toJsUndo ())

        CM.Redo ->
            model
                |> Update.pure
                |> Update.do (toJsRedo ())

        CM.New ->
            if isEditingNew model && isSelectedBlank model then
                { model | editorMode = CommandMode }
                    |> overCursor ItemTree.deleteIfBlankAndLeaf
                    |> Update.pure

            else
                generateId model
                    |> Update.map
                        (\( id, newModel ) ->
                            overCursor (ItemTree.appendNew id) newModel
                                |> setEditingNew
                        )
                    |> Update.andThen ensureFocus

        CM.Edit ->
            { model | editorMode = EditSelected E_Existing }
                |> Update.pure

        CM.Delete ->
            model |> updateCursorAndCacheWithHistory ItemTree.delete

        CM.CollapseOrPrev ->
            model |> updateCursorAndCacheWithHistory ItemTree.collapseOrParent

        CM.ExpandOrNext ->
            model |> updateCursorAndCacheWithHistory ItemTree.expandOrNext

        CM.Prev ->
            updateCursorAndCache ItemTree.backward model

        CM.Next ->
            updateCursorAndCache ItemTree.forward model

        CM.MoveUp ->
            updateCursorAndCacheWithHistory ItemTree.moveUp model

        CM.MoveDown ->
            updateCursorAndCacheWithHistory ItemTree.moveDown model

        CM.Outdent ->
            updateCursorAndCacheWithHistory ItemTree.outdent model

        CM.Indent ->
            updateCursorAndCacheWithHistory ItemTree.indent model

        CM.RotateActionable ->
            updateCursorAndCacheWithHistory ItemTree.rotateActionable model


handleEditMsg : EditModeMsg -> Model -> ( Model, Cmd Msg )
handleEditMsg msg model =
    case msg of
        EM.New ->
            if isEditingNew model && isSelectedBlank model then
                { model | editorMode = CommandMode }
                    |> overCursor ItemTree.deleteIfBlankAndLeaf
                    |> Update.pure

            else
                generateId model
                    |> Update.map
                        (\( id, newModel ) ->
                            overCursor (ItemTree.appendNew id) newModel
                                |> setEditingNew
                        )
                    |> Update.andThen ensureFocus

        EM.Save ->
            if isEditingNew model && isSelectedBlank model then
                { model | editorMode = CommandMode }
                    |> overCursor ItemTree.deleteIfBlankAndLeaf
                    |> Update.pure

            else
                { model | editorMode = CommandMode }
                    |> updateCursorAndCacheWithHistory ItemTree.deleteIfBlankAndLeaf

        EM.Cancel ->
            if isEditingNew model && isSelectedBlank model then
                { model | editorMode = CommandMode }
                    |> overCursor ItemTree.deleteIfBlankAndLeaf
                    |> Update.pure

            else
                { model | editorMode = CommandMode }
                    |> updateCursorAndCacheWithHistory ItemTree.deleteIfBlankAndLeaf

        EM.LineChanged newContent ->
            updateCursorAndCacheWithHistory (ItemTree.setContent newContent) model


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


updateCursorAndCache : (ItemZipper -> ItemZipper) -> Model -> ( Model, Cmd Msg )
updateCursorAndCache fn model =
    overCursor fn model
        |> persistIfChanged OnlyCache model


updateCursorAndCacheWithHistory : (ItemZipper -> ItemZipper) -> Model -> ( Model, Cmd Msg )
updateCursorAndCacheWithHistory fn model =
    overCursor fn model
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


ensureFocus model =
    let
        domIdFn =
            case model.editorMode of
                EditSelected _ ->
                    fragInputDomId

                CommandMode ->
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
            [ ( HotKey.is "Enter", CM.New )
            , ( HotKey.is " ", CM.Edit )
            , ( HotKey.isCtrl " ", CM.RotateActionable )

            --            , ( HotKey.isShift "Enter", NOP )
            , ( HotKey.is "ArrowUp", CM.Prev )
            , ( HotKey.is "ArrowDown", CM.Next )
            , ( HotKey.isMeta "ArrowUp", CM.MoveUp )
            , ( HotKey.isMeta "ArrowDown", CM.MoveDown )
            , ( HotKey.isShift "Tab", CM.Outdent )
            , ( HotKey.is "Tab", CM.Indent )
            , ( HotKey.is "ArrowLeft", CM.CollapseOrPrev )
            , ( HotKey.is "ArrowRight", CM.ExpandOrNext )
            , ( HotKey.is "Delete", CM.Delete )
            , ( HotKey.isMeta "z", CM.Undo )
            , ( HotKey.isKeyMetaShift "z", CM.Redo )
            ]
                |> List.map (overSecond (CMMsgReceived >> addSecond True))
    in
    labelKeyMap
        |> List.Extra.find (Tuple.first >> applyTo ke)
        |> Maybe.map Tuple.second


fragmentEditorHotKeyDecoder : KeyEvent -> Maybe ( Msg, Bool )
fragmentEditorHotKeyDecoder ke =
    let
        inputKeyMap : List ( KeyEvent -> Bool, ( Msg, Bool ) )
        inputKeyMap =
            [ ( HotKey.is "Enter", EM.New )
            , ( HotKey.isMeta "Enter", EM.Save )
            , ( HotKey.is "Escape", EM.Cancel )

            --            , ( HotKey.isShift "Enter", NOP )
            --            , ( HotKey.isShift "Tab", EM.Outdent )
            --            , ( HotKey.is "Tab", EM.Indent )
            ]
                |> List.map (overSecond (EMMsgReceived >> addSecond True))
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
            isEditingSelected model

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


type ChildCollapsedState
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


viewLine : TreeViewModel -> ItemTree -> Html Msg
viewLine vm tree =
    let
        childState =
            getChildrenState tree

        prefix =
            case childState of
                Expanded ->
                    "+"

                Collapsed ->
                    "-"

                NoChildren ->
                    "o"

        additionalStyles =
            if childState == NoChildren || isRootTree tree vm then
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


viewFragment : TreeViewModel -> ItemTree -> Html Msg
viewFragment vm tree =
    let
        isSel =
            isSelectedTree tree vm
    in
    Html.Styled.toUnstyled <|
        UI.Tree.viewFragment
            { text = ItemTree.treeFragment tree
            , isRoot = isRootTree tree vm
            , isSelected = isSel
            , attrs =
                List.map Html.Styled.Attributes.fromUnstyled
                    [ id <| fragDomId <| Item.Tree.id tree
                    , attrIf isSel (tabindex 0)
                    , HotKey.preventDefaultOnKeyDownEvent fragmentHotKeyDecoder
                    ]
            }


viewFragmentEditor : TreeViewModel -> ItemTree -> Html Msg
viewFragmentEditor _ tree =
    input
        [ id <| fragInputDomId <| Item.Tree.id tree
        , cx [ ph1, mr1, w_100, bn ]
        , value <| ItemTree.treeFragment tree
        , onInput <| EMMsgReceived << EM.LineChanged
        , HotKey.preventDefaultOnKeyDownEvent fragmentEditorHotKeyDecoder
        ]
        []
