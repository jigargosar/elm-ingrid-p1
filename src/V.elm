module V exposing (attrIf, btn, cc, co, cx, rr, t, tInt, viewIf)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Tachyons exposing (classes)
import Tachyons.Classes exposing (..)


btn attrs =
    button (classes [ br1, bw1, ba, b__light_blue, ma1, pv1, ph2, mw_100, w3 ] :: attrs)


rr classNames =
    div [ classes <| ([ flex, flex_row ] ++ classNames) ]


cc classNames =
    div [ classes <| [ flex ] ++ classNames, style "flex-basis" "100%", style "flex" "1" ]


co classNames =
    div [ classes <| [ center ] ++ classNames ]


tInt i =
    text <| String.fromInt i


t =
    text


cx =
    classes


noHtml =
    t ""


noAttr =
    Html.Attributes.attribute "noAttr" ""


attrIf bool a =
    if bool then
        a

    else
        noAttr


viewIf bool v =
    if bool then
        v

    else
        noHtml
