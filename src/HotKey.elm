module HotKey exposing (KeyEvent, is, isKeyMetaShift, isMeta, isShift, metaModifier, metaShiftModifier, noModifiers, shiftModifier)


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


isKeyMetaShift key keyEvent =
    keyEvent.key == key && metaShiftModifier keyEvent


isMeta key keyEvent =
    keyEvent.key == key && metaModifier keyEvent
