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


view : Model -> Html Msg
view model =
    co [ sans_serif, ma0, min_vh_100, flex, flex_column ]
        [ t "Story Book"
        , viewItemLabel "I M L" True
        ]


viewItemLabel labelText isRoot =
    div [ cx [ pa3, ma3, f4 ] ] [ t labelText ]
