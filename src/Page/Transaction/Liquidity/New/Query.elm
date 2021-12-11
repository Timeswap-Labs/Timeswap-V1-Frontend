module Page.Transaction.Liquidity.New.Query exposing (givenNew)

import Data.Chain as Chain exposing (Chain)
import Data.Pool as Pool exposing (Pool)
import Data.Uint as Uint exposing (Uint)
import Json.Encode as Encode exposing (Value)


type alias QueryNew =
    { chainId : Chain
    , pool : Pool
    , spot : Maybe Uint
    , assetIn : Uint
    , debtOut : Uint
    , collateralOut : Uint
    }


givenNew : QueryNew -> Value
givenNew { chainId, pool, spot, assetIn, debtOut, collateralOut } =
    [ ( "chainId", chainId |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "spot"
      , spot
            |> Maybe.map Uint.encode
            |> Maybe.withDefault Encode.null
      )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "debtOut", debtOut |> Uint.encode )
    , ( "collateralOut", collateralOut |> Uint.encode )
    ]
        |> Encode.object
