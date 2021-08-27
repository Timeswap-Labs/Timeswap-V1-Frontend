module Data.Backdrop exposing (Backdrop, setBackdrop)


type Backdrop
    = Supported
    | NotSupported


setBackdrop : Bool -> Backdrop
setBackdrop hasBackdropSupport =
    if hasBackdropSupport == True then
        Supported

    else
        NotSupported
