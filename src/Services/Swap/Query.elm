module Services.Swap.Query exposing (Query, Return, decoder, encode)

import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Services.Swap.GameToken as GameToken exposing (GameToken)
import Url.Parser exposing (query)


type alias Query =
    { token1 : GameToken
    , token2 : GameToken
    , amount : Uint
    }


type alias Return =
    { token1 : GameToken
    , token2 : GameToken
    , amount : Uint
    , result : Uint
    }


encode : Query -> Value
encode { token1, token2, amount } =
    Encode.object
        [ ( "token1", token1 |> GameToken.encode )
        , ( "token2", token2 |> GameToken.encode )
        , ( "amount", amount |> Uint.encode )
        ]


decoder : Decoder Return
decoder =
    Decode.succeed Return
        |> Pipeline.required "token1" GameToken.decoder
        |> Pipeline.required "token2" GameToken.decoder
        |> Pipeline.required "amount" Uint.decoder
        |> Pipeline.required "result" Uint.decoder
