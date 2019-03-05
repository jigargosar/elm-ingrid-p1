module Item exposing (Item)


type alias Item =
    { id : String
    , title : String
    , pid : Maybe String
    , childIds : List String
    }
