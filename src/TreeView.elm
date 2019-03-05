module TreeView exposing (ItemLabelProps, concatIf, ifElse, ter, viewItemLabel)

import Html exposing (Html, div)
import Tachyons.Classes exposing (..)
import V exposing (cx, t)


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
