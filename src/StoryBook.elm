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


concatIf : Bool -> List a -> List a -> List a
concatIf bool l1 l2 =
    if bool then
        l1 ++ l2

    else
        l2


ifElse cFn tFn fFn val =
    if cFn val then
        tFn val

    else
        fFn val


ter : Bool -> a -> a -> a
ter bool v1 v2 =
    if bool then
        v1

    else
        v2


viewItemLabel props =
    let
        nonRootC =
            [ pa1, bg_white ]

        rootC =
            [ f4, pa2, bg_white ]

        selectedC =
            [ bg_light_red, white ]

        finalC =
            ter props.isRoot rootC nonRootC
                |> concatIf props.isSelected selectedC
    in
    div [ cx finalC ] [ t props.text ]


view : Model -> Html Msg
view model =
    Story.viewBook
        [ rootItemLabelStories
        , itemLabelStories
        ]


itemLabelStories =
    let
        defaultP =
            { text = "I am vanilla Item Label", isSelected = False, isRoot = False }

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


rootItemLabelStories =
    let
        defaultP =
            { text = "Root", isSelected = False, isRoot = True }

        selectedP =
            { defaultP | isSelected = True }
    in
    Story.of_ "ItemLabel"
        [ Story.add "unselected root" <|
            viewItemLabel defaultP
        , Story.add "selected root" <|
            viewItemLabel selectedP
        , Story.add "long unselected root" <|
            viewItemLabel { defaultP | text = "I am long unselected ROOT label" }
        , Story.add "long selected root" <|
            viewItemLabel { selectedP | text = "I am long selected ROOT label" }
        ]
