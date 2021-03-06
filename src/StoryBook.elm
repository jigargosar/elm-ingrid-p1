module StoryBook exposing (main)

import Browser
import Html exposing (Html, div)
import Story
import Tachyons.Classes exposing (..)
import TreeView
import V exposing (co, cx, t)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    {}


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( {}, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        []



-- UPDATE


type Msg
    = NOP


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NOP ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Story.viewBook
        [ rootItemLabelStories
        , itemLabelStories
        ]


itemLabelStories =
    let
        defaultP =
            { text = "I am vanilla Item Label", isSelected = False, isRoot = False, attrs = [] }

        selectedP =
            { defaultP | isSelected = True }
    in
    Story.of_ "ItemLabel"
        [ Story.add "with defaultProps" <|
            TreeView.viewFragment defaultP
        , Story.add "with selected" <|
            TreeView.viewFragment { selectedP | text = "I should be Selected" }
        , Story.add "with another defaultProps example" <|
            TreeView.viewFragment { defaultP | text = "I am another plain label" }
        ]


rootItemLabelStories =
    let
        defaultP =
            { text = "Root", isSelected = False, isRoot = True, attrs = [] }

        selectedP =
            { defaultP | isSelected = True }
    in
    Story.of_ "ItemLabel"
        [ Story.add "unselected root" <|
            TreeView.viewFragment defaultP
        , Story.add "selected root" <|
            TreeView.viewFragment selectedP
        , Story.add "long unselected root" <|
            TreeView.viewFragment { defaultP | text = "I am long unselected ROOT label" }
        , Story.add "long selected root" <|
            TreeView.viewFragment { selectedP | text = "I am long selected ROOT label" }
        ]
