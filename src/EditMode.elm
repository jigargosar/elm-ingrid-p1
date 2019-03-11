module EditMode exposing (EditModeMsg(..))


type EditModeMsg
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
