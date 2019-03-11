module CommandMode exposing (CommandModeMsg(..))


type CommandModeMsg
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
