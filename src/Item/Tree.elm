module Item.Tree exposing (ItemTree, decoder, encoder, id)

import Item exposing (Item)
import Json.Decode exposing (Decoder)
import Json.Encode
import Tree exposing (Tree)


type alias ItemTree =
    Tree Item


encoder : ItemTree -> Json.Encode.Value
encoder tree =
    let
        item =
            Tree.label tree

        children =
            Tree.children tree
    in
    Json.Encode.object
        [ ( "label", Item.encoder item )
        , ( "children", Json.Encode.list encoder children )
        ]



{-
   --decoder : Decoder ItemTree
   --decoder =
   --    Json.Decode.map2 (\label children -> Tree.singleton label |> Tree.replaceChildren children)
   --        (Json.Decode.field "label" Json.Decode.string)
   --        (Json.Decode.field "children" (Json.Decode.list decoder))

-}


decoder : Decoder ItemTree
decoder =
    Json.Decode.map2 (\label children -> Tree.singleton label |> Tree.replaceChildren children)
        (Json.Decode.field "label" Item.decoder)
        (Json.Decode.lazy (\_ -> Json.Decode.field "children" (Json.Decode.list decoder)))


id =
    Tree.label >> .id
