module Modal.Swap.Query exposing (PriceQuery, Return, decoder, encode)

import Data.Token as Token exposing (Token)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)


type alias PriceQuery =
    { token1 : Token
    , token2 : Token
    }


type alias Return =
    { incomingTokenId : String
    , outgoingTokenId : String
    , incomingTokenPrice : Float
    , outgoingTokenPrice : Float
    , relativeTokenPrice : Float
    }


encode : PriceQuery -> Value
encode { token1, token2 } =
    Encode.object
        [ ( "incomingTokenId", token1 |> Token.encode )
        , ( "outgoingTokenId", token2 |> Token.encode )
        ]


decoder : Decoder Return
decoder =
    Decode.succeed Return
        |> Pipeline.required "incomingTokenId" Decode.string
        |> Pipeline.required "outgoingTokenId" Decode.string
        |> Pipeline.required "incomingTokenPrice" Decode.float
        |> Pipeline.required "outgoingTokenPrice" Decode.float
        |> Pipeline.required "relativeTokenPrice" Decode.float
