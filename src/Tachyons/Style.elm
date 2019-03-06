module Tachyons.Style exposing (br1, dib, f4, lh_copy, lh_solid, lh_title, outline_0, pa1, ph2)

import Css exposing (rem)


dib =
    Css.display Css.inlineBlock


lh_title =
    Css.lineHeight (Css.num 1.25)


lh_solid =
    Css.lineHeight (Css.num 1)


lh_copy =
    Css.lineHeight (Css.num 1.5)


su1 =
    Css.rem 0.25


su2 =
    Css.rem 0.5


pa1 =
    Css.padding su1


phN su =
    Css.batch [ Css.paddingLeft su, Css.paddingRight su ]


ph2 =
    phN su2


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


outline_0 =
    Css.outline Css.none
