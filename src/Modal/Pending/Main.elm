module Modal.Pending.Main exposing (Msg, update)


type Msg
    = Exit


update : Msg -> Maybe ()
update msg =
    case msg of
        Exit ->
            Nothing
