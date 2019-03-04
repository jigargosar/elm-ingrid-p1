port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events exposing (onKeyDown)
import HotKey exposing (KeyEvent)
import Html exposing (Html, div)
import ItemLookup exposing (Item, ItemLookup)
import ItemTree exposing (ItemTreeCursor)
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


focusInputCmd =
    Browser.Dom.focus "master-input"
        |> Task.attempt (Debug.log "focusing master input" >> (\_ -> NOP))


initEditingMode model =
    ( { model | viewMode = EditingSelected }
    , Cmd.batch [ focusInputCmd ]
    )


edit model =
    if model.viewMode == EditingSelected then
        ( model
        , Cmd.batch []
        )

    else
        initEditingMode model


moveUp model =
    ( overCursor ItemTree.moveUp model
    , Cmd.batch []
    )


moveDown model =
    ( overCursor ItemTree.moveDown model
    , Cmd.batch []
    )


indent model =
    ( overCursor ItemTree.indent model
    , Cmd.batch []
    )


outdent model =
    ( overCursor ItemTree.outdent model
    , Cmd.batch []
    )


overCursor : (ItemTreeCursor -> ItemTreeCursor) -> Model -> Model
overCursor fn model =
    { model | cursor = fn model.cursor }


selectBackward model =
    ( overCursor ItemTree.backward model, Cmd.none )


selectForward model =
    ( overCursor ItemTree.forward model, Cmd.none )


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


appendNewAndStartEditing =
    withNewId
        (\id model ->
            Update.pure
                { model
                    | cursor = ItemTree.appendNew id model.cursor
                }
                |> Update.andThen initEditingMode
        )


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
    co [ sans_serif, "us-none", ma0 ]
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
    in
    div [ classes [ pl3 ] ]
        [ viewRootTreeItem selected root
        , viewItemForest selected (ItemTree.treeChildren root)
        ]


viewItemForest selected children =
    div [ classes [ pl3 ] ] (children |> List.map (viewItemTree selected))


viewItemTree selected tree =
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
    div []
        [ div [ classes [ h2, flex, items_center ] ]
            [ div [ classes labelClasses ]
                [ t <| ItemTree.treeFragment tree, t " ", t <| ItemTree.treeId tree ]
            ]
        , viewItemForest selected (ItemTree.treeChildren tree)
        ]


viewRootTreeItem selected root =
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
