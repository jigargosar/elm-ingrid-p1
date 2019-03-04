port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events exposing (onKeyDown)
import HotKey exposing (KeyEvent)
import Html exposing (Html, div, input, textarea)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput)
import Html.Keyed
import ItemLookup exposing (Item, ItemLookup)
import ItemTree exposing (ItemTree, ItemTreeCursor)
import Json.Decode exposing (Decoder)
import List.Extra
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
    { itemLookup : ItemLookup
    , maybeFocusedItemId : Maybe String
    , cursor : ItemTreeCursor
    , viewMode : ViewMode
    , seed : Random.Seed
    }


type alias Flags =
    { items : List Item, maybeFocusedItemId : Maybe String, now : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    update InitReceived
        { itemLookup = ItemLookup.fromList flags.items
        , maybeFocusedItemId = flags.maybeFocusedItemId
        , cursor = ItemTree.initialCursor
        , viewMode = Navigating
        , seed = Random.initialSeed flags.now
        }


getItems model =
    model.itemLookup |> ItemLookup.toList



-- SUBSCRIPTIONS


keyEventDecoder : Decoder KeyEvent
keyEventDecoder =
    Json.Decode.map5 KeyEvent
        (Json.Decode.at [ "key" ] Json.Decode.string)
        (Json.Decode.at [ "ctrlKey" ] Json.Decode.bool)
        (Json.Decode.at [ "metaKey" ] Json.Decode.bool)
        (Json.Decode.at [ "shiftKey" ] Json.Decode.bool)
        (Json.Decode.at [ "altKey" ] Json.Decode.bool)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown <| Json.Decode.map KeyDownReceived keyEventDecoder
        ]



-- UPDATE


type Msg
    = NOP
    | KeyDownReceived KeyEvent
    | InitReceived
    | ContentChanged String


getItemDomId : Item -> String
getItemDomId item =
    "item-id-" ++ item.id


cacheNewModel model =
    toJsCache { items = getItems model, maybeFocusedItemId = model.maybeFocusedItemId }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NOP ->
            ( model, Cmd.none )

        ContentChanged newContent ->
            ( overCursor (ItemTree.setContent newContent) model, Cmd.none )

        InitReceived ->
            ( model
            , Cmd.batch
                []
            )

        KeyDownReceived keyEvent ->
            let
                _ =
                    Debug.log "KeyDownReceived" keyEvent
            in
            {- if keyEvent.meta then
                   case keyEvent.key of
                       "ArrowLeft" ->
                           onUnnestFocused model

                       "ArrowRight" ->
                           onNestFocused model

                       "ArrowUp" ->
                           moveFocusedBy -1 model

                       "ArrowDown" ->
                           moveFocusedBy 1 model

                       _ ->
                           ( model, Cmd.none )

               else
                   ( model, Cmd.none )
            -}
            globalKeyMap
                |> List.Extra.find (Tuple.first >> applyTo keyEvent)
                |> Maybe.map (\( _, mFn ) -> mFn model)
                |> Maybe.withDefault ( model, Cmd.none )


applyTo =
    (|>)


globalKeyMap : List ( KeyEvent -> Bool, Model -> ( Model, Cmd Msg ) )
globalKeyMap =
    [ ( HotKey.is "Enter", appendNewAndStartEditing )
    , ( HotKey.is " ", edit )
    , ( HotKey.isShift "Enter", prependNewAndStartEditing )
    , ( HotKey.is "ArrowUp", selectBackward )
    , ( HotKey.is "ArrowDown", selectForward )
    , ( HotKey.isMeta "ArrowLeft", outdent )
    , ( HotKey.isMeta "ArrowRight", indent )
    , ( HotKey.isMeta "ArrowUp", moveUp )
    , ( HotKey.isMeta "ArrowDown", moveDown )
    ]


navKeyMap : List ( KeyEvent -> Bool, Model -> ( Model, Cmd Msg ) )
navKeyMap =
    [ ( HotKey.is "Enter", appendNewAndStartEditing )
    , ( HotKey.is " ", edit )
    , ( HotKey.isShift "Enter", prependNewAndStartEditing )
    , ( HotKey.is "ArrowUp", selectBackward )
    , ( HotKey.is "ArrowDown", selectForward )
    , ( HotKey.isMeta "ArrowLeft", outdent )
    , ( HotKey.isMeta "ArrowRight", indent )
    , ( HotKey.isMeta "ArrowUp", moveUp )
    , ( HotKey.isMeta "ArrowDown", moveDown )
    ]


editKeyMap : List ( KeyEvent -> Bool, Model -> ( Model, Cmd Msg ) )
editKeyMap =
    [ ( HotKey.is "Enter", appendNewAndStartEditing )
    , ( HotKey.is " ", edit )
    , ( HotKey.isShift "Enter", prependNewAndStartEditing )
    , ( HotKey.is "ArrowUp", selectBackward )
    , ( HotKey.is "ArrowDown", selectForward )
    , ( HotKey.isMeta "ArrowLeft", outdent )
    , ( HotKey.isMeta "ArrowRight", indent )
    , ( HotKey.isMeta "ArrowUp", moveUp )
    , ( HotKey.isMeta "ArrowDown", moveDown )
    ]


focusInputCmd model =
    Browser.Dom.focus (getEditInputDomId <| ItemTree.getSelectedTree model.cursor)
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


overCursor : (ItemTreeCursor -> ItemTreeCursor) -> Model -> Model
overCursor fn model =
    { model | cursor = fn model.cursor }


selectBackward model =
    Update.pure (overCursor ItemTree.backward model)
        |> Update.effect ensureEditInputFocusCmd


selectForward model =
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


appendNewAndStartEditing : Model -> ( Model, Cmd Msg )
appendNewAndStartEditing =
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
        , viewCursor model
        ]


viewShortcutHint label shortcut =
    div [ classes [ avenir, ph2, f6, lh_title, br1, ba, b__black_20 ] ]
        [ div [ classes [] ] [ t label ]
        , div [ classes [ light_red ] ] [ t shortcut ]
        ]


viewCursor model =
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
        [ viewRootTreeItem isEditing selected root
        , viewItemForest isEditing selected (ItemTree.treeChildren root)
        ]


viewItemForest : Bool -> ItemTree -> List ItemTree -> Html Msg
viewItemForest isEditing selected forest =
    div [ classes [ pl3 ] ]
        (forest
            |> List.map (viewItemTree isEditing selected)
        )


viewItemLabel selected tree =
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


getEditInputDomId : ItemTree -> String
getEditInputDomId tree =
    "item-edit-input-dom-id-" ++ ItemTree.treeId tree


viewEditItemLabel tree =
    let
        content =
            ItemTree.treeFragment tree
    in
    div [ classes [ dib, mv1, pa2, br1, bg_white ] ]
        [ div [ classes [ dib, relative, "pre-wrap", "break-word" ], style "min-width" "10rem" ]
            [ textarea
                [ Html.Attributes.id (getEditInputDomId tree)
                , classes
                    [ pa0
                    , bn
                    , absolute
                    , "resize-none"

                    --                    , o_50
                    , w_100
                    , h_100
                    , outline_0

                    --                , "pre-wrap"
                    --                , "break-word"
                    ]
                , value content
                , onInput ContentChanged
                ]
                []
            , div [ classes [ dib ] ] [ t content ]
            ]
        ]


viewItemTree isEditing selected tree =
    div []
        [ if isEditing && selected == tree then
            viewEditItemLabel tree

          else
            viewItemLabel selected tree
        , viewItemForest isEditing selected (ItemTree.treeChildren tree)
        ]


viewRootTreeItem isEditing selected root =
    let
        labelClasses =
            let
                defaultClasses =
                    [ pa1, dib, f3, br1 ]

                selectedClasses =
                    [ bg_light_red, white ]
            in
            if root == selected then
                defaultClasses ++ selectedClasses

            else
                defaultClasses
    in
    div [ classes [ pv2 ] ]
        [ div [ classes labelClasses ]
            [ t <| ItemTree.treeFragment root ]
        ]
