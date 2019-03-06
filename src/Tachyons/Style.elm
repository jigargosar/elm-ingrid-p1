module Tachyons.Style exposing
    ( bgDodgerblue
    , bgDodgerblueA
    , bg_lightblue
    , blackA
    , br1
    , dib
    , f4
    , lh_copy
    , lh_solid
    , lh_title
    , noOutline
    , pa
    , ph
    , white
    )

import Array
import Css exposing (Style, color, hex, hsla, rem, rgba)
import Tachyons.Color as Color


dib =
    Css.display Css.inlineBlock


lh_solid =
    Css.lineHeight (Css.num 1)


lh_title =
    Css.lineHeight (Css.num 1.25)


lh_copy =
    Css.lineHeight (Css.num 1.5)


spacingScale =
    [ 0, 0.25, 0.5, 1, 2, 4, 8, 16 ]
        |> Array.fromList


spacingUnitFromN : Int -> Css.Rem
spacingUnitFromN n =
    let
        newN =
            clamp 0 7 n
    in
    Array.get newN spacingScale
        |> Maybe.withDefault 0
        |> rem


pa =
    Css.padding << spacingUnitFromN


phN su =
    Css.batch [ Css.paddingLeft su, Css.paddingRight su ]


ph =
    phN << spacingUnitFromN


f4 =
    Css.fontSize (Css.rem 1.25)


bru1 =
    rem 0.125


bru2 =
    rem 0.25


bru3 =
    rem 0.5


bru4 =
    rem 1


brN brUnit =
    Css.borderRadius brUnit


br1 =
    brN bru1


noOutline =
    Css.outline Css.none


colorHex =
    color << hex


white =
    colorHex "#fff"


bg_S =
    Css.property "background-color"


bgDodgerblue =
    Css.backgroundColor Color.dodgerblue


bgDodgerblueA =
    Css.backgroundColor << Color.dodgerblueA


blackA : Float -> Style
blackA pct =
    color <| rgba 0 0 0 (pct / 100)


bg_lightblue =
    bg_S "lightblue"
