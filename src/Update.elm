module Update exposing (andThen, pure)


andThen fn ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            fn model
    in
    ( newModel, Cmd.batch [ cmd, newCmd ] )


pure model =
    ( model, Cmd.none )
