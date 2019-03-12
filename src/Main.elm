port module Main exposing (main)

import BasicsX exposing (..)
import Browser
import Browser.DomX as Dom
import Browser.Events
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
import NormalMode as NM exposing (NormalModeMsg)
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


type ToJs
    = CacheCursor Json.Encode.Value
    | Undo
    | Redo
    | PersistHistory Json.Encode.Value


sendToJs : ToJs -> Cmd msg
sendToJs msg =
    let
        encodeAndSend msgName payload =
            Json.Encode.object
                [ ( "msg", Json.Encode.string msgName )
                , ( "payload", payload )
                ]
                |> toJs
    in
    case msg of
        CacheCursor cursor ->
            encodeAndSend "cache" <| Json.Encode.object [ ( "cursor", cursor ) ]

        Undo ->
            encodeAndSend "undo" <| Json.Encode.null

        Redo ->
            encodeAndSend "redo" <| Json.Encode.null

        PersistHistory val ->
            encodeAndSend "persistHistory" val


port fromJs : (Json.Encode.Value -> msg) -> Sub msg


type FromJs
    = ErrorReceived Err
    | HistoryDocReceived Json.Encode.Value


port onJsLoadFromCouchHistory : (Json.Encode.Value -> msg) -> Sub msg


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type EditItemType
    = E_New
    | E_Existing


type InputMode
    = Normal
    | EditingSelected EditItemType


type alias Model =
    { cursor : ItemZipper
    , inputMode : InputMode
    , seed : Random.Seed
    , toasties : Toasty.Stack Toasties.Toast
    }


type alias Flags =
    { cache : { cursor : Json.Encode.Value }, now : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    update (Init flags)
        { cursor = ItemTree.initialCursor
        , inputMode = Normal
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
    { model | inputMode = EditingSelected E_New }


setNormalMode model =
    { model | inputMode = Normal }



-- LIB CONFIG


toastyConfig : Toasty.Config msg
toastyConfig =
    Toasties.config



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown <| Json.Decode.map GlobalKeyDown HotKey.keyEventDecoder
        , onJsLoadFromCouchHistory LoadFromCouchHistory
        , fromJs JsMsgReceived
        ]



-- UPDATE


type Msg
    = DomFocusResultReceived (Result String ())
    | GlobalKeyDown KeyEvent
    | Init Flags
    | LoadFromCouchHistory Json.Decode.Value
    | JsMsgReceived Json.Decode.Value
    | ToastyMsg (Toasty.Msg Toasties.Toast)
    | OnJsError Err
    | EditModeMsgReceived EditModeMsg
    | NormalModeMsgReceived NormalModeMsg


fragDomId : String -> String
fragDomId itemId =
    "item-id-" ++ itemId


fragInputDomId : String -> String
fragInputDomId itemId =
    "item-input-dom-id-" ++ itemId


isEditingSelected : Model -> Bool
isEditingSelected model =
    case model.inputMode of
        EditingSelected _ ->
            True

        Normal ->
            False


errDecoder : Decoder Err
errDecoder =
    Json.Decode.list Json.Decode.string
        |> Json.Decode.andThen
            (\errorStrings ->
                case errorStrings of
                    fst :: rest ->
                        Json.Decode.succeed
                            ( fst
                            , List.head rest
                                |> Maybe.withDefault "<no error description provided>"
                            )

                    _ ->
                        Json.Decode.fail "Empty Payload for fromJs msg 'error'"
            )


fromJsDecoder : Decoder FromJs
fromJsDecoder =
    Json.Decode.field "msg" Json.Decode.string
        |> Json.Decode.andThen
            (\msg ->
                let
                    payloadDecoder =
                        Json.Decode.field "payload"
                in
                case msg of
                    "error" ->
                        payloadDecoder errDecoder
                            |> Json.Decode.map ErrorReceived

                    "history" ->
                        payloadDecoder Json.Decode.value
                            |> Json.Decode.map HistoryDocReceived

                    _ ->
                        Json.Decode.fail <| "Invalid msg" ++ msg
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ToastyMsg subMsg ->
            Toasty.update toastyConfig ToastyMsg subMsg model

        OnJsError err ->
            model |> addErrorToast err

        JsMsgReceived encodedMsg ->
            let
                handleDecodeError : Json.Decode.Error -> ModelCmd
                handleDecodeError decodeError =
                    addErrorToast ( "Cursor Decode Error", errorToString decodeError ) model

                handleJsMsg msg =
                    case msg of
                        ErrorReceived err ->
                            addErrorToast err model

                        HistoryDocReceived encodedCursor ->
                            loadEncodedCursorAndCache encodedCursor model
                                |> Update.andThen ensureFocus
            in
            Json.Decode.decodeValue fromJsDecoder encodedMsg
                |> Result.Extra.unpack handleDecodeError handleJsMsg

        DomFocusResultReceived (Err msg) ->
            model |> addErrorToast ( "DomFocusError", msg )

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

        NormalModeMsgReceived msg ->
            handleCommandMsg msg model

        EditModeMsgReceived msg ->
            handleEditMsg msg model


handleCommandMsg msg model =
    case msg of
        NM.Undo ->
            ( model, sendToJs Undo )

        NM.Redo ->
            ( model, sendToJs Redo )

        NM.New ->
            ( (addNewLeaf >> setEditingNew) model, Cmd.none )

        NM.Edit ->
            ( { model | inputMode = EditingSelected E_Existing }, Cmd.none )

        NM.Delete ->
            model |> updateCursorAndCacheWithHistory ItemTree.delete

        NM.CollapseOrPrev ->
            model |> updateCursorAndCacheWithHistory ItemTree.collapseOrParent

        NM.ExpandOrNext ->
            model |> updateCursorAndCacheWithHistory ItemTree.expandOrNext

        NM.Prev ->
            updateCursorAndCache ItemTree.backward model

        NM.Next ->
            updateCursorAndCache ItemTree.forward model

        NM.MoveUp ->
            updateCursorAndCacheWithHistory ItemTree.moveUp model

        NM.MoveDown ->
            updateCursorAndCacheWithHistory ItemTree.moveDown model

        NM.Outdent ->
            updateCursorAndCacheWithHistory ItemTree.outdent model

        NM.Indent ->
            updateCursorAndCacheWithHistory ItemTree.indent model

        NM.RotateActionable ->
            updateCursorAndCacheWithHistory ItemTree.rotateActionable model


isEditingNewBlankAndLeaf : Model -> Bool
isEditingNewBlankAndLeaf model =
    model.inputMode
        == EditingSelected E_New
        && ItemTree.isFragmentBlank model.cursor
        && ItemTree.isLeaf model.cursor


handleEditMsg : EditModeMsg -> Model -> ( Model, Cmd Msg )
handleEditMsg msg model =
    case msg of
        EM.New ->
            ifElse isEditingNewBlankAndLeaf
                (overCursor ItemTree.delete >> setNormalMode)
                (addNewLeaf >> setEditingNew)
                model
                |> Update.pure

        EM.Save ->
            ifElse isEditingNewBlankAndLeaf
                (setNormalMode >> overCursor ItemTree.delete >> Update.pure)
                (setNormalMode
                    >> updateCursorAndCacheWithHistory ItemTree.deleteIfBlankAndLeaf
                )
                model

        EM.Cancel ->
            handleEditMsg EM.Save model

        EM.LineChanged newContent ->
            updateCursorAndCacheWithHistory (ItemTree.setContent newContent) model


type alias Err =
    ( String, String )


addToast : Toasties.Toast -> Model -> ModelCmd
addToast toast model =
    Update.pure model
        |> Toasty.addToast toastyConfig ToastyMsg toast


addErrorToast : Err -> Model -> ModelCmd
addErrorToast ( title, detail ) =
    Toasties.Error title detail |> addToast


type alias ModelCmd =
    ( Model, Cmd Msg )


loadEncodedCursorAndCache : Json.Encode.Value -> Model -> ( Model, Cmd Msg )
loadEncodedCursorAndCache encodedCursor model =
    let
        handleCursorDecodeError error =
            addErrorToast ( "Cursor Decode Error", errorToString error ) model

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
                sendToJs <| CacheCursor encodedCursor
        in
        ( newModel
        , case persistenceType of
            OnlyCache ->
                cacheCmd

            CacheAndHistory ->
                Cmd.batch [ cacheCmd, sendToJs <| PersistHistory encodedCursor ]
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
    ( randomId, newModel )


mapModelWithNewId fn =
    generateId >> (\( id, model ) -> fn id model)


addNewLeaf =
    mapModelWithNewId (\id -> overCursor (ItemTree.appendNew id))


ensureFocus model =
    let
        domIdFn =
            case model.inputMode of
                EditingSelected _ ->
                    fragInputDomId

                Normal ->
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
            [ ( HotKey.is "Enter", NM.New )
            , ( HotKey.is " ", NM.Edit )
            , ( HotKey.isCtrl " ", NM.RotateActionable )
            , ( HotKey.is "ArrowUp", NM.Prev )
            , ( HotKey.is "ArrowDown", NM.Next )
            , ( HotKey.isMeta "ArrowUp", NM.MoveUp )
            , ( HotKey.isMeta "ArrowDown", NM.MoveDown )
            , ( HotKey.isShift "Tab", NM.Outdent )
            , ( HotKey.is "Tab", NM.Indent )
            , ( HotKey.is "ArrowLeft", NM.CollapseOrPrev )
            , ( HotKey.is "ArrowRight", NM.ExpandOrNext )
            , ( HotKey.is "Delete", NM.Delete )
            , ( HotKey.isMeta "z", NM.Undo )
            , ( HotKey.isKeyMetaShift "z", NM.Redo )
            ]
                |> List.map (overSecond (NormalModeMsgReceived >> addSecond True))
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

            --            , ( HotKey.isShift "Tab", EM.Outdent )
            --            , ( HotKey.is "Tab", EM.Indent )
            ]
                |> List.map (overSecond (EditModeMsgReceived >> addSecond True))
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
        , onInput <| EditModeMsgReceived << EM.LineChanged
        , HotKey.preventDefaultOnKeyDownEvent fragmentEditorHotKeyDecoder
        ]
        []
