module Item.TreeOp exposing (TreeOp(..))

import Item.Tree exposing (ItemTree)


type alias Location =
    { parentId : String, index : Int }


type TreeOp
    = MoveFocus { fromId : String, toId : String }
    | Insert Location ItemTree
    | Delete Location ItemTree
    | Move { id : String, fromLocation : Location, toLocation : Location }
    | EditFragment { id : String, before : String, after : String }
    | Expand { id : String }
    | Collapse { id : String }
