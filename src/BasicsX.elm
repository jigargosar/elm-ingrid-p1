module BasicsX exposing (applyTo, concatIf, defaultEmptyStringTo, eqs, ifElse, tap, ter)


eqs =
    (==)


concatIf : Bool -> List a -> List a -> List a
concatIf bool l1 l2 =
    if bool then
        l2 ++ l1

    else
        l2


ifElse cFn tFn fFn val =
    if cFn val then
        tFn val

    else
        fFn val


ter : Bool -> a -> a -> a
ter bool v1 v2 =
    if bool then
        v1

    else
        v2


applyTo =
    (|>)


defaultEmptyStringTo def str =
    if String.isEmpty str then
        def

    else
        str


tap fn v =
    let
        _ =
            fn v
    in
    v
