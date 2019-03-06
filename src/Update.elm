module Update exposing (andThen, map, pure)


pure model =
    ( model, Cmd.none )


andThen fn ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            fn model
    in
    ( newModel, Cmd.batch [ cmd, newCmd ] )


map fn ( model, cmd ) =
    ( fn model, cmd )
