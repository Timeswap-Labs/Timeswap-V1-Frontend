module Modal.Swap.Transaction exposing (Transaction, encode)

import Data.Token as Token exposing (Token)
import Json.Encode as Encode exposing (Value)


type alias Transaction =
    { incomingTokenId : Token
    , outgoingTokenId : Token
    , incomingTokenQty : String
    , userAddress : String
    , signature : String
    }


encode : Transaction -> Value
encode { incomingTokenId, outgoingTokenId, incomingTokenQty, userAddress, signature } =
    Encode.object
        [ ( "incomingTokenId", incomingTokenId |> Token.toApiTokenId |> Encode.string )
        , ( "outgoingTokenId", outgoingTokenId |> Token.toApiTokenId |> Encode.string )
        , ( "incomingTokenQty", incomingTokenQty |> Encode.string )
        , ( "userAddress", userAddress |> Encode.string )
        , ( "signature", signature |> Encode.string )
        ]
