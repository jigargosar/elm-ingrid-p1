module UI.Tree exposing (FragmentProps, viewFragment)

import BasicsX exposing (..)
import Html exposing (Html, div)
import Tachyons.Classes exposing (..)
import V exposing (cx, t)


type alias FragmentProps msg =
    { text : String
    , isRoot : Bool
    , isSelected : Bool
    , attrs : List (Html.Attribute msg)
    }


viewFragment : FragmentProps msg -> Html msg
viewFragment props =
    let
        nonRootC =
            [ dib, ph1, bg_white, br1 ]

        rootC =
            [ dib, pv2, f4, bg_white, br1 ]

        selectedC =
            [ bg_light_red, white ]

        finalC =
            ter props.isRoot rootC nonRootC
                |> concatIf props.isSelected selectedC

        renderText =
            defaultEmptyStringTo "Untitled" props.text
    in
    div ([ cx finalC ] ++ props.attrs) [ t renderText ]
