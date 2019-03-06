module Item.Zipper exposing (ItemZipper, decoder, encoder)

import BasicsX exposing (..)
import Item exposing (Item)
import Item.Tree
import Json.Decode exposing (Decoder)
import Json.Encode
import Tree.Zipper as Zipper exposing (Zipper)


type alias ItemZipper =
    Zipper Item


encoder : ItemZipper -> Json.Encode.Value
encoder z =
    Json.Encode.object
        [ ( "root", z |> Zipper.root >> Zipper.tree >> Item.Tree.encoder )
        , ( "focusedItemId", z |> id >> Json.Encode.string )
        ]


decoder : Decoder ItemZipper
decoder =
    Json.Decode.map2
        (\tree fid ->
            let
                z =
                    Zipper.fromTree tree
            in
            Zipper.findFromRoot (.id >> eqs fid) z
                |> Maybe.withDefault z
        )
        (Json.Decode.field "root" Item.Tree.decoder)
        (Json.Decode.field "focusedItemId" Json.Decode.string)


id =
    Zipper.tree >> Item.Tree.id
