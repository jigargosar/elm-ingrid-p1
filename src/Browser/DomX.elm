module Browser.DomX exposing (focus)

import Browser.Dom
import StringX as String
import Task exposing (Task)


focus : String -> Task String ()
focus domId =
    if String.isBlank domId then
        Task.fail "Focus Error: empty domId"

    else
        Browser.Dom.focus domId
            |> Task.mapError (\_ -> "Focus Error: domId not found, " ++ domId)
