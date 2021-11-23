module Services.Swap.Query exposing (Query, Return, decoder, encode)

import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Services.Swap.GameToken as GameToken exposing (GameToken)


type alias Query =
    { token1 : GameToken
    , token2 : GameToken
    , amount : Uint
    }


type alias Return =
    { incomingTokenPrice : Float
    , outgoingTokenPrice : Float
    , relativeTokenPrice : Float
    }


encode : Query -> Value
encode { token1, token2, amount } =
    Encode.object
        [ ( "incomingTokenId", token1 |> GameToken.encode )
        , ( "outgoingTokenId", token2 |> GameToken.encode )
        , ( "incomingTokenQty", amount |> Uint.encode )
        ]


decoder : Decoder Return
decoder =
    Decode.succeed Return
        |> Pipeline.required "incomingTokenPrice" Decode.float
        |> Pipeline.required "outgoingTokenPrice" Decode.float
        |> Pipeline.required "relativeTokenPrice" Decode.float
