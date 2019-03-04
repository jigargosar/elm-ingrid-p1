module Update exposing (andThen, effect, pure)


andThen fn ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            fn model
    in
    ( newModel, Cmd.batch [ cmd, newCmd ] )


pure model =
    ( model, Cmd.none )


effect fn ( model, cmd ) =
    ( model, Cmd.batch [ cmd, fn model ] )
