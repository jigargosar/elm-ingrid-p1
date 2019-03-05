module UI.TreeView exposing (ItemLabelProps, viewFragment)

import BasicsX exposing (..)
import Html exposing (Html, div)
import Tachyons.Classes exposing (..)
import V exposing (cx, t)


type alias ItemLabelProps msg =
    { text : String
    , isRoot : Bool
    , isSelected : Bool
    , attrs : List (Html.Attribute msg)
    }


viewFragment : ItemLabelProps msg -> Html msg
viewFragment props =
    let
        nonRootC =
            [ dib, pa1, bg_white, br1 ]

        rootC =
            [ dib, pa2, f4, bg_white, br1 ]

        selectedC =
            [ bg_light_red, white ]

        finalC =
            ter props.isRoot rootC nonRootC
                |> concatIf props.isSelected selectedC

        renderText =
            defaultEmptyStringTo "Untitled" props.text
    in
    div ([ cx finalC ] ++ props.attrs) [ t renderText ]
