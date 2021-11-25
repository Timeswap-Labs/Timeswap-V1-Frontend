module Services.Swap.Transaction exposing (Transaction, encode)

import Json.Encode as Encode exposing (Value)
import Services.Swap.GameToken as GameToken exposing (GameToken)


type alias Transaction =
    { incomingTokenId : GameToken
    , outgoingTokenId : GameToken
    , incomingTokenQty : Float
    , userAddress : String
    }


encode : Transaction -> Value
encode { incomingTokenId, outgoingTokenId, incomingTokenQty, userAddress } =
    Encode.object
        [ ( "incomingTokenId", incomingTokenId |> GameToken.encode )
        , ( "outgoingTokenId", outgoingTokenId |> GameToken.encode )
        , ( "incomingTokenQty", incomingTokenQty |> Encode.float )
        , ( "userAddress", userAddress |> Encode.string )
        ]
