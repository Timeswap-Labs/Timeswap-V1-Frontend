module Services.Swap.Transaction exposing (Transaction, encode)

import Json.Encode as Encode exposing (Value)
import Services.Swap.GameToken as GameToken exposing (GameToken)


type alias Transaction =
    { incomingTokenId : GameToken
    , outgoingTokenId : GameToken
    , incomingTokenQty : String
    , userAddress : String
    , signature : String
    }


encode : Transaction -> Value
encode { incomingTokenId, outgoingTokenId, incomingTokenQty, userAddress, signature } =
    Encode.object
        [ ( "incomingTokenId", incomingTokenId |> GameToken.encode )
        , ( "outgoingTokenId", outgoingTokenId |> GameToken.encode )
        , ( "incomingTokenQty", incomingTokenQty |> Encode.string )
        , ( "userAddress", userAddress |> Encode.string )
        , ( "signature", signature |> Encode.string )
        ]
