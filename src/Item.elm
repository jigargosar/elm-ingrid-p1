module Item exposing (Fragment, Item, decoder, encoder)

import Json.Decode exposing (Decoder)
import Json.Encode


type alias Fragment =
    String


type alias Item =
    { id : String
    , fragment : Fragment
    , collapsed : Bool
    }


encoder : Item -> Json.Encode.Value
encoder item =
    Json.Encode.object
        [ ( "id", Json.Encode.string item.id )
        , ( "fragment", Json.Encode.string item.fragment )
        , ( "collapsed", Json.Encode.bool item.collapsed )
        ]


decoder : Decoder Item
decoder =
    Json.Decode.map3 Item
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "fragment" Json.Decode.string)
        (Json.Decode.field "collapsed" Json.Decode.bool)
