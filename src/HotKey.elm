module HotKey exposing (KeyEvent, is, isCtrl, isKeyMetaShift, isMeta, isShift, keyEventDecoder, metaModifier, metaShiftModifier, noModifiers, preventDefaultOnKeyDownEvent, shiftModifier)

import Html
import Html.Events
import Json.Decode exposing (Decoder)


type alias KeyEvent =
    { key : String
    , ctrl : Bool
    , meta : Bool
    , shift : Bool
    , alt : Bool
    }


noModifiers : KeyEvent -> Bool
noModifiers keyEvent =
    not (keyEvent.ctrl || keyEvent.meta || keyEvent.shift || keyEvent.alt)


shiftModifier : KeyEvent -> Bool
shiftModifier keyEvent =
    keyEvent.shift && not (keyEvent.ctrl || keyEvent.meta || keyEvent.alt)


metaShiftModifier : KeyEvent -> Bool
metaShiftModifier keyEvent =
    keyEvent.meta && keyEvent.shift && not (keyEvent.ctrl || keyEvent.alt)


metaModifier : KeyEvent -> Bool
metaModifier keyEvent =
    keyEvent.meta && not (keyEvent.ctrl || keyEvent.alt || keyEvent.shift)


is key keyEvent =
    keyEvent.key == key && noModifiers keyEvent


isShift key keyEvent =
    keyEvent.key == key && shiftModifier keyEvent


isCtrl key keyEvent =
    keyEvent.key == key && keyEvent.ctrl && not (keyEvent.shift || keyEvent.meta || keyEvent.alt)


isKeyMetaShift key keyEvent =
    keyEvent.key == key && metaShiftModifier keyEvent


isMeta key keyEvent =
    keyEvent.key == key && metaModifier keyEvent


preventDefaultOnKeyDownEvent : (KeyEvent -> Maybe ( msg, Bool )) -> Html.Attribute msg
preventDefaultOnKeyDownEvent keyEventToMaybePreventDefault =
    Html.Events.preventDefaultOn "keydown"
        (Json.Decode.andThen
            (keyEventToMaybePreventDefault
                >> Maybe.map Json.Decode.succeed
                >> Maybe.withDefault (Json.Decode.fail "Not Interested")
            )
            keyEventDecoder
        )


keyEventDecoder : Decoder KeyEvent
keyEventDecoder =
    Json.Decode.map5 KeyEvent
        (Json.Decode.at [ "key" ] Json.Decode.string)
        (Json.Decode.at [ "ctrlKey" ] Json.Decode.bool)
        (Json.Decode.at [ "metaKey" ] Json.Decode.bool)
        (Json.Decode.at [ "shiftKey" ] Json.Decode.bool)
        (Json.Decode.at [ "altKey" ] Json.Decode.bool)
