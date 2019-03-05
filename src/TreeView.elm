module TreeView exposing (ItemLabelProps, viewItemLabel)

import BasicsX exposing (..)
import Html exposing (Html, div)
import Tachyons.Classes exposing (..)
import V exposing (cx, t)


type alias ItemLabelProps =
    { text : String
    , isRoot : Bool
    , isSelected : Bool
    }


viewItemLabel : ItemLabelProps -> Html msg
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
