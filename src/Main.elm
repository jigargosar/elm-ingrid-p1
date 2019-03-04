port module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, button, div, span, table)
import Html.Attributes exposing (attribute, tabindex)
import ItemLookup exposing (Item, ItemLookup)
import ItemTree exposing (ItemTreeCursor)
import Json.Decode exposing (Decoder)
import List.Extra
import Random exposing (Generator)
import Tachyons exposing (classes)
import Tachyons.Classes exposing (..)
import Tree.Zipper
import V exposing (co, noHtml, t)


port toJsCache : { items : List Item, maybeFocusedItemId : Maybe String } -> Cmd msg


port bulkItemDocs : List Item -> Cmd msg


port newItemDoc : ( Item, Int ) -> Cmd msg



--port debouncedBulkItemDocs : List Item -> Cmd msg


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


getRootItemsOrEmpty model =
    model.itemLookup |> ItemLookup.getRootItems |> Maybe.withDefault []


getDisplayRootItems model =
    case model.maybeDndItems of
        Just items ->
            items

        Nothing ->
            getRootItemsOrEmpty model


getItemById id model =
    ItemLookup.getById id model.itemLookup


getRootItem : Model -> Maybe Item
getRootItem model =
    ItemLookup.getRoot model.itemLookup



-- SUBSCRIPTIONS


type alias KeyEvent =
    { key : String
    , ctrl : Bool
    , meta : Bool
    , shift : Bool
    , alt : Bool
    }


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
    | AddItemClicked
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

        AddItemClicked ->
            getRootItem model
                |> Maybe.map
                    (\rootItem ->
                        ( model
                        , Cmd.batch
                            [ newItemDoc ( rootItem, List.length rootItem.childIds )
                            ]
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

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


noModifiers : KeyEvent -> Bool
noModifiers keyEvent =
    not (keyEvent.ctrl || keyEvent.meta || keyEvent.shift || keyEvent.alt)


shiftModifier : KeyEvent -> Bool
shiftModifier keyEvent =
    keyEvent.shift && not (keyEvent.ctrl || keyEvent.meta || keyEvent.alt)


keyIs key keyEvent =
    keyEvent.key == key && noModifiers keyEvent


keyIsShift key keyEvent =
    keyEvent.key == key && shiftModifier keyEvent


globalKeyMap : List ( KeyEvent -> Bool, Model -> ( Model, Cmd Msg ) )
globalKeyMap =
    [ ( keyIs "Enter", appendNewAndStartEditing )
    , ( keyIsShift "Enter", prependNewAndStartEditing )
    , ( keyIs "ArrowUp", selectBackward )
    , ( keyIs "ArrowDown", selectForward )
    ]


overCursor : (ItemTreeCursor -> ItemTreeCursor) -> Model -> Model
overCursor fn model =
    { model | cursor = fn model.cursor }


selectBackward model =
    ( overCursor ItemTree.backward model, Cmd.none )


selectForward model =
    ( overCursor ItemTree.forward model, Cmd.none )


appendNewAndStartEditing model =
    let
        idGen : Generator String
        idGen =
            Random.int 999999999 Random.maxInt
                |> Random.map String.fromInt

        ( id, newSeed ) =
            Random.step idGen model.seed
    in
    ( { model
        | cursor = ItemTree.appendNew id model.cursor
        , seed = newSeed
        , viewMode = EditingSelected
      }
    , Cmd.none
    )


prependNewAndStartEditing model =
    let
        idGen : Generator String
        idGen =
            Random.int 999999999 Random.maxInt
                |> Random.map String.fromInt

        ( id, newSeed ) =
            Random.step idGen model.seed
    in
    ( { model
        | cursor = ItemTree.prependNew id model.cursor
        , seed = newSeed
        , viewMode = EditingSelected
      }
    , Cmd.none
    )


moveFocusedBy offset model =
    let
        updateIdx parent focusedItemIdx =
            let
                offsetIdx =
                    focusedItemIdx + offset

                maybeNewIdx =
                    if offsetIdx >= List.length parent.childIds || offsetIdx < 0 then
                        Nothing

                    else
                        Just offsetIdx
            in
            maybeNewIdx
                |> Maybe.map
                    (\finalIdx ->
                        List.Extra.swapAt focusedItemIdx finalIdx parent.childIds
                            |> (\newChildIds -> { parent | childIds = newChildIds })
                    )
    in
    model.maybeFocusedItemId
        |> Maybe.andThen
            (\id ->
                ItemLookup.getParentOfId id model.itemLookup
                    |> Maybe.andThen
                        (\parent ->
                            parent.childIds
                                |> List.Extra.findIndex ((==) id)
                                |> Maybe.andThen (updateIdx parent)
                        )
            )
        |> Maybe.map (\updatedItem -> ( model, bulkItemDocs [ updatedItem ] ))
        |> Maybe.withDefault ( model, Cmd.none )


onNestFocused model =
    let
        updateParents : String -> Item -> Item -> List Item
        updateParents id oldParent newParent =
            [ { oldParent | childIds = List.filter ((/=) id) oldParent.childIds }
            , { newParent | childIds = newParent.childIds ++ [ id ] }
            ]
    in
    model.maybeFocusedItemId
        |> Maybe.andThen
            (\id ->
                ItemLookup.getParentAndPrevPrevSibOf id model.itemLookup
            )
        |> Maybe.map
            (\( id, oldParent, newParent ) ->
                updateParents id oldParent newParent
                    |> (\updatedItems ->
                            ( model, bulkItemDocs updatedItems )
                       )
            )
        |> Maybe.withDefault ( model, Cmd.none )


onUnnestFocused model =
    let
        updateParents : String -> Item -> Item -> List Item
        updateParents id parent grandParent =
            grandParent.childIds
                |> List.Extra.findIndex ((==) parent.id)
                |> Maybe.map
                    (\parentIdx ->
                        [ { parent | childIds = List.filter ((/=) id) parent.childIds }
                        , { grandParent
                            | childIds =
                                List.Extra.splitAt (parentIdx + 1) grandParent.childIds
                                    |> (\( pre, post ) -> pre ++ [ id ] ++ post)
                          }
                        ]
                    )
                |> Maybe.withDefault []
    in
    model.maybeFocusedItemId
        |> Maybe.andThen
            (\id ->
                ItemLookup.getParentAndGrandParentOf id model.itemLookup
            )
        |> Maybe.map
            (\( id, parent, grandParent ) ->
                updateParents id parent grandParent
                    |> (\updatedItems ->
                            ( model, bulkItemDocs updatedItems )
                       )
            )
        |> Maybe.withDefault ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    co [ sans_serif, "us-none", ma0 ]
        [ div [ classes [ flex, bg_black, white ] ]
            [ viewShortcutHint "Line Below" "Enter"
            , viewShortcutHint "Line Above" "Shift+Enter"
            ]
        , viewCursor model

        --        , viewTree model
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
            ItemTree.selectedTree model.cursor
    in
    div [ classes [ pl3 ] ]
        [ viewRootTreeItem selected root
        , viewItemForest selected (ItemTree.treeChildren root)
        ]


viewItemForest selected children =
    div [ classes [ pl3 ] ] (children |> List.map (viewTreeItem selected))


viewTreeItem selected tree =
    let
        labelClasses =
            let
                defaultClasses =
                    [ pa1, dib, br1 ]

                selectedClasses =
                    [ bg_light_red, white ]

                notSelectedClasses =
                    []

                hasVisibleChildrenClasses =
                    [ bb, bw2, b__black_30 ]
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
