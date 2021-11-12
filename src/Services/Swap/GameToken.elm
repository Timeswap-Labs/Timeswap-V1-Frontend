module Services.Swap.GameToken exposing (GameToken(..), encode)

import Json.Encode as Encode exposing (Value)


type GameToken
    = Token1
    | Token2
    | Token3


encode : GameToken -> Value
encode gameToken =
    (case gameToken of
        Token1 ->
            "0x1212412434"

        Token2 ->
            "0ds32423423"

        Token3 ->
            "0xcvg23124325"
    )
        |> Encode.string
