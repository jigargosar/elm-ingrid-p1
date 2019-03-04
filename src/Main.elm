port module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, button, div, span)
import Html.Attributes exposing (attribute, tabindex)
import ItemLookup exposing (Item, ItemLookup)
import ItemTree exposing (ItemTreeCursor)
import Json.Decode exposing (Decoder)
import List.Extra
import Tachyons exposing (classes)
import Tachyons.Classes exposing (..)
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
    }


type alias Flags =
    { items : List Item, maybeFocusedItemId : Maybe String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    update InitReceived
        { itemLookup = ItemLookup.fromList flags.items
        , maybeFocusedItemId = flags.maybeFocusedItemId
        , cursor = ItemTree.initialCursor
        , viewMode = Navigating
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
    }


keyEventDecoder : Decoder KeyEvent
keyEventDecoder =
    Json.Decode.map3 KeyEvent
        (Json.Decode.at [ "key" ] Json.Decode.string)
        (Json.Decode.at [ "ctrlKey" ] Json.Decode.bool)
        (Json.Decode.at [ "metaKey" ] Json.Decode.bool)


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
            {- let
                   _ =
                       Debug.log "KeyDownReceived" keyEvent
               in
            -}
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
            case keyEvent.key of
                "Enter" ->
                    if model.viewMode == Navigating then
                        ( { model | viewMode = EditingSelected, cursor = ItemTree.append model.cursor }, Cmd.none )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


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
    co [ sans_serif, pt3, "us-none" ]
        [ div []
            [ button [ classes [ br1, ba, b__black_20, ttu, mh1, f6 ] ] [ t "add new : press enter" ]
            ]
        , viewCursor model

        --        , viewTree model
        ]


viewCursor model =
    let
        root =
            ItemTree.currentRoot model.cursor

        rootFragment =
            ItemTree.nodeFragment root

        selectedNode =
            ItemTree.selectedNode model.cursor

        isRootSelected =
            selectedNode == root

        fragmentClasses =
            let
                defaultClasses =
                    [ pa1, f3, bb, bw2, b__black_30, fw4 ]

                selectedClasses =
                    [ bg_light_red, white, br ]
            in
            if isRootSelected then
                defaultClasses ++ selectedClasses

            else
                defaultClasses
    in
    div [ classes [ pa3 ] ]
        [ span [ classes fragmentClasses ]
            [ t rootFragment ]
        ]


viewTree model =
    let
        mRoot : Maybe Item
        mRoot =
            getRootItem model

        getChildrenOfId : String -> List Item
        getChildrenOfId id =
            ItemLookup.getChildrenOfId id model.itemLookup
                |> Maybe.withDefault []

        getChildren : Item -> List Item
        getChildren item =
            getChildrenOfId item.id

        viewItemTitle : Item -> Html Msg
        viewItemTitle item =
            div
                [ classes [ mv2, pa3, ba, b__black_50, br1 ]
                , tabindex 0
                , Html.Attributes.id <| getItemDomId item
                , attribute "data-is-focusable" "true"
                ]
                [ t item.title ]

        viewChildren : Item -> List (Html Msg)
        viewChildren item =
            List.map viewItem (getChildren item)

        viewItem : Item -> Html Msg
        viewItem item =
            div []
                [ viewItemTitle item
                , div [ classes [ ml4 ] ] (viewChildren item)
                ]
    in
    mRoot
        |> Maybe.map (viewChildren >> div [])
        |> Maybe.withDefault noHtml
