module Story exposing (Story, add, storyContainer, viewStories, viewStory)

import Html exposing (Html, div)
import Tachyons.Classes exposing (..)
import V exposing (co, cx, t)


viewStories storyList =
    co [ sans_serif, ma0, min_vh_100, flex, flex_column ]
        [ t "Story Book"
        , div [ cx [ flex, flex_column, flex_grow_1 ] ] <|
            List.map viewStory storyList
        ]


type alias Story msg =
    { title : String
    , view : Html msg
    }


add : String -> Html msg -> Story msg
add title view_ =
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
