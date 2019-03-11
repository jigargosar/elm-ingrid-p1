module CommandMode exposing (CommandModeMsg(..))


type CommandModeMsg
    = LineChanged String
    | New
    | Save
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
