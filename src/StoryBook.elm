module StoryBook exposing (main)

import Browser
import Html exposing (Html, div)
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
    let
        stories =
            [ story "ItemLabel with defaultProps" <|
                viewItemLabel { text = "I am default vanilla Item Label", isSelected = False, isRoot = False }
            , story "ItemLabel with selected" <|
                viewItemLabel { text = "I should be Selected", isSelected = True, isRoot = False }
            , story "ItemLabel with another defaultProps example" <|
                viewItemLabel { text = "I am another plain label", isSelected = False, isRoot = False }
            ]
    in
    viewStoryBook stories


viewStoryBook storyList =
    co [ sans_serif, ma0, min_vh_100, flex, flex_column ]
        [ t "Story Book"
        , div [ cx [ flex, flex_column, flex_grow_1 ] ] <|
            List.map viewStory storyList
        ]


type alias Story msg =
    { title : String
    , view : Html msg
    }


story : String -> Html msg -> Story msg
story title view_ =
    { title = title, view = view_ }


storyContainer =
    div
        [ cx
            [ flex_grow_1
            , pa3
            , ma3
            , flex
            , items_center
            , justify_center
            , bg_light_gray
            , ba
            , b__black_50
            , br1
            ]
        ]


viewStory s =
    div [ cx [] ]
        [ t s.title
        , storyContainer [ s.view ]
        ]
