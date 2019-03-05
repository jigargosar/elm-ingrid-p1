module TreeView exposing (ItemLabelProps, viewItemLabel)

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


viewItemLabel : ItemLabelProps msg -> Html msg
viewItemLabel props =
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
    in
    div [ cx finalC ] [ t props.text ]
