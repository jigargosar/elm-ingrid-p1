module UI.Tree exposing (FragmentProps, viewFragment)

import BasicsX exposing (..)
import Css exposing (focus)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import SV exposing (cx, t)
import Tachyons.Style exposing (..)


type alias FragmentProps msg =
    { text : String
    , isRoot : Bool
    , isSelected : Bool
    , attrs : List (Html.Attribute msg)
    }


isBlank =
    String.trim >> String.isEmpty


viewFragment : FragmentProps msg -> Html msg
viewFragment p =
    let
        baseStyles : List Css.Style
        baseStyles =
            [ dib, lh_title, ph 2, blackA 0.8 ]

        rootStyles =
            [ f4 ]

        selectedStyles =
            [ noOutline, br1, bgDodgerblueA 0.5, white, focus [ bgDodgerblueA 0.8, white ] ]

        dimStyles =
            [ blackA 0.5 ]

        selectedDimStyle =
            [ focus [ whiteA 0.5 ] ]

        renderText =
            defaultEmptyStringTo (ter p.isRoot "Root" "Untitled") p.text

        shouldDim =
            isBlank p.text && not p.isRoot

        finalCss =
            baseStyles
                |> concatIf p.isRoot rootStyles
                |> concatIf p.isSelected selectedStyles
                |> concatIf shouldDim dimStyles
                |> concatIf (shouldDim && p.isSelected) selectedDimStyle
    in
    div ([ css finalCss ] ++ p.attrs) [ t <| Debug.log "renderText" renderText ]



--viewTreeContainer model =
--    let
--        root =
--            ItemTree.rootTree model.cursor
--
--        selected =
--            ItemTree.getSelectedTree model.cursor
--
--        isEditing =
--            model.viewMode == EditingSelected
--
--        containerClasses =
--            let
--                baseClasses =
--                    [ flex_grow_1, pl3 ]
--
--                editingModeClasses =
--                    [ bg_black_20, black_50 ]
--            in
--            if isEditing then
--                baseClasses ++ editingModeClasses
--
--            else
--                baseClasses
--    in
--    div [ classes containerClasses ]
--        [ viewRootItemTreeLabel isEditing selected root
--        , viewItemForest isEditing selected (ItemTree.treeChildren root)
--        ]
--
--
--viewItemForest : Bool -> ItemTree -> List ItemTree -> Html Msg
--viewItemForest isEditingMode selected forest =
--    div [ classes [ pl3 ] ]
--        (forest
--            |> List.map (viewItemTree isEditingMode selected)
--        )
--
--
--viewItemTree isEditingMode selected tree =
--    div []
--        [ if isEditingMode && selected == tree then
--            viewEditItemLabel tree
--
--          else
--            viewItemTreeLabel selected tree
--        , viewItemForest isEditingMode selected (ItemTree.treeChildren tree)
--        ]
--
--
--viewItemTreeLabel selected tree =
--    let
--        labelClasses =
--            let
--                defaultClasses =
--                    [ pa1, dib, br1 ]
--
--                selectedClasses =
--                    [ bg_light_red, white ]
--
--                notSelectedClasses =
--                    []
--            in
--            if tree == selected then
--                defaultClasses ++ selectedClasses
--
--            else
--                defaultClasses ++ notSelectedClasses
--    in
--    div [ classes [ h2, flex, items_center ] ]
--        [ div [ classes labelClasses ]
--            [ t <| ItemTree.treeFragment tree, t " ", t <| ItemTree.treeId tree ]
--        ]
--
--
--viewRootItemTreeLabel isEditing selected rootTree =
--    let
--        labelClasses =
--            let
--                defaultClasses =
--                    [ pa1, dib, f3, br1 ]
--
--                selectedClasses =
--                    [ bg_light_red, white ]
--            in
--            if rootTree == selected then
--                defaultClasses ++ selectedClasses
--
--            else
--                defaultClasses
--    in
--    div [ classes [ pv2 ] ]
--        [ div [ classes labelClasses ]
--            [ t <| ItemTree.treeFragment rootTree ]
--        ]
--
--
--
--viewEditItemLabel tree =
--    let
--        content =
--            ItemTree.treeFragment tree
--    in
--    div [ classes [ dib, mv1, pa2, br1, bg_white ] ]
--        [ div [ classes [ dib, relative, "pre-wrap", "break-word" ], style "min-width" "10rem" ]
--            [ textarea
--                [ Html.Attributes.id (getItemTreeInputDomId tree)
--                , classes
--                    [ pa0
--                    , bn
--                    , absolute
--                    , "resize-none"
--
--                    --                    , o_50
--                    , w_100
--                    , h_100
--                    , outline_0
--                    , "pre-wrap"
--                    , "break-word"
--                    ]
--                , value content
--                , onInput LineChanged
--                , HotKey.preventDefaultOnKeyDownEvent itemEditorHotKeyDispatcher
--                ]
--                []
--            , div [ classes [ dib ], style "min-width" "10rem" ] [ t content ]
--            ]
--        ]
