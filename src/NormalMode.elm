module NormalMode exposing (NormalModeMsg(..))


type NormalModeMsg
    = New
    | Prev
    | Edit
    | Next
    | MoveUp
    | MoveDown
    | Outdent
    | Indent
    | CollapseOrPrev
    | ExpandOrNext
    | Delete
    | RotateActionable
    | Undo
    | Redo
