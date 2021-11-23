module Data.Backdrop exposing (Backdrop, Flag, init)


type Backdrop
    = Supported
    | NotSupported


type alias Flag =
    Bool


init : Flag -> Backdrop
init hasBackdropSupport =
    if hasBackdropSupport then
        Supported

    else
        NotSupported
