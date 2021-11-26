port module Modal.ChainList.Main exposing (Msg, update)

import Data.Chain as Chain exposing (Chain)
import Json.Encode exposing (Value)


type Msg
    = ClickChain Chain
    | Exit


update : Msg -> ( Maybe Never, Cmd Msg )
update msg =
    case msg of
        ClickChain chain ->
            ( Nothing
            , chain
                |> Chain.encode
                |> changeChain
            )

        Exit ->
            ( Nothing
            , Cmd.none
            )


port changeChain : Value -> Cmd msg
