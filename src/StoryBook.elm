module StoryBook exposing (main)

import Browser
import Html exposing (Html, div)
import Story
import Tachyons.Classes exposing (..)
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


type alias ItemLabelProps =
    { text : String
    , isRoot : Bool
    , isSelected : Bool
    }


viewItemLabel props =
    div [ cx [ pa3, ma3, f4, bg_white ] ] [ t props.text ]


view : Model -> Html Msg
view model =
    Story.viewBook
        [ itemLabelStories
        ]


itemLabelStories =
    Story.of_ "ItemLabel"
        [ Story.add "with defaultProps" <|
            viewItemLabel { text = "I am default vanilla Item Label", isSelected = False, isRoot = False }
        , Story.add "with selected" <|
            viewItemLabel { text = "I should be Selected", isSelected = True, isRoot = False }
        , Story.add "with another defaultProps example" <|
            viewItemLabel { text = "I am another plain label", isSelected = False, isRoot = False }
        ]
