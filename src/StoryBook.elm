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
    let
        defaultC =
            [ pa3, ma3, f4, bg_white ]

        selectedC =
            [ bg_light_red, white ]

        finalC =
            if props.isSelected then
                defaultC ++ selectedC

            else
                defaultC
    in
    div [ cx finalC ] [ t props.text ]


view : Model -> Html Msg
view model =
    Story.viewBook
        [ itemLabelStories
        ]


itemLabelStories =
    let
        defaultP =
            { text = "I am default vanilla Item Label", isSelected = False, isRoot = False }

        selectedP =
            { defaultP | isSelected = True }
    in
    Story.of_ "ItemLabel"
        [ Story.add "with defaultProps" <|
            viewItemLabel defaultP
        , Story.add "with selected" <|
            viewItemLabel { selectedP | text = "I should be Selected" }
        , Story.add "with another defaultProps example" <|
            viewItemLabel { defaultP | text = "I am another plain label" }
        ]
