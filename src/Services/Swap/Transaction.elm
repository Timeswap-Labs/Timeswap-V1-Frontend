module Services.Swap.Transaction exposing (Transaction, encode)

import Json.Encode as Encode exposing (Value)
import Services.Swap.GameToken as GameToken exposing (GameToken)


type alias Transaction =
    { inToken : GameToken
    , outToken : GameToken
    , amount : String
    }


encode : Transaction -> Value
encode { inToken, outToken, amount } =
    [ ( "inToken", inToken |> GameToken.encode )
    , ( "outToken", outToken |> GameToken.encode )
    , ( "amount", amount |> Encode.string )
    ]
        |> Encode.object
