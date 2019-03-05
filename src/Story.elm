module Story exposing (Story, add, of_, storyContainer, viewBook, viewStory)

import Html exposing (Html, div)
import Tachyons.Classes exposing (..)
import V exposing (co, cx, t)


type alias Story msg =
    { title : String
    , view : Html msg
    }


type alias StoryGroup msg =
    { title : String
    , stories : List (Story msg)
    }


viewBook storyGroups =
    co [ sans_serif, ma0, min_vh_100, flex, flex_column ]
        [ t "Story Book"
        , div [ cx [ flex, flex_column, flex_grow_1 ] ] <|
            List.map viewStoryGroup storyGroups
        ]


viewStoryGroup sg =
    div [ cx [] ]
        [ div [ cx [ pa3 ] ] [ t sg.title ]
        , div [ cx [ flex, flex_column, flex_grow_1 ] ] <|
            List.map viewStory sg.stories
        ]


add : String -> Html msg -> Story msg
add title view_ =
    { title = title, view = view_ }


storyContainer =
    div
        [ cx
            [ flex_grow_1

            --            , pa3
            , ma3
            , flex

            --            , items_center
            --            , justify_center
            , bg_light_gray
            , ba
            , b__black_50
            , b__dashed
            , br1
            ]
        ]


viewStory s =
    div [ cx [] ]
        [ t s.title
        , storyContainer [ s.view ]
        ]


of_ title stories =
    { title = title, stories = stories }
