module Update exposing (andThen, do, map, pure)


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


do newCmd ( model, cmd ) =
    ( model, Cmd.batch [ cmd, newCmd ] )
