module Modal.Pay.Query exposing (Proportion, Sum, proportion, sum)

import Blockchain.User.Due as Due exposing (Due)
import Blockchain.User.TokenId as TokenId exposing (TokenId)
import Data.Chain as Chain exposing (Chain)
import Data.Pool as Pool exposing (Pool)
import Data.Uint as Uint exposing (Uint)
import Json.Encode as Encode exposing (Value)


type alias Proportion =
    { chain : Chain
    , pool : Pool
    , tokenId : TokenId
    , due : Due
    , assetIn : Uint
    }


type alias Sum =
    { chain : Chain
    , pool : Pool
    , collateralsOut : List Uint
    }


proportion : Proportion -> Value
proportion { chain, pool, tokenId, due, assetIn } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "tokenId", tokenId |> TokenId.encode )
    , ( "due", due |> Due.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    ]
        |> Encode.object


sum : Sum -> Value
sum { chain, pool, collateralsOut } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "collateralsOut", collateralsOut |> Encode.list Uint.encode )
    ]
        |> Encode.object
