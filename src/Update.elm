module Update exposing (andThen)


andThen fn ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            fn model
    in
    ( newModel, Cmd.batch [ cmd, newCmd ] )
