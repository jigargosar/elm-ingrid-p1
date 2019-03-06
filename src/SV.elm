module SV exposing (btn, cc, co, cx, noHtml, rr, t, tInt, viewIf)

import Html.Styled as Html exposing (Html, button, div, text)
import Html.Styled.Attributes exposing (class, style)
import Tachyons.Classes exposing (..)


btn attrs =
    button (cx [ br1, bw1, ba, b__light_blue, ma1, pv1, ph2, mw_100, w3 ] :: attrs)


rr classNames =
    div [ cx <| ([ flex, flex_row ] ++ classNames) ]


cc classNames =
    div [ cx <| [ flex ] ++ classNames, style "flex-basis" "100%", style "flex" "1" ]


co classNames =
    div [ cx <| [ center ] ++ classNames ]


tInt i =
    text <| String.fromInt i


t =
    text


cx =
    String.join " " >> class


noHtml =
    t ""


viewIf bool v =
    if bool then
        v

    else
        noHtml
