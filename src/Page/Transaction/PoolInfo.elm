module Page.Transaction.PoolInfo exposing (PoolInfo, decoder, encode)

import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)


type alias PoolInfo =
    { x : Uint
    , y : Uint
    , z : Uint
    , assetReserve : Uint
    , collateralReserve : Uint
    , totalLiquidity : Uint
    , totalBond : Uint
    , totalInsurance : Uint
    , totalDebtCreated : Uint
    , spot : Maybe Uint
    }


decoder : Decoder PoolInfo
decoder =
    Decode.succeed PoolInfo
        |> Pipeline.required "x" Uint.decoder
        |> Pipeline.required "y" Uint.decoder
        |> Pipeline.required "z" Uint.decoder
        |> Pipeline.required "assetReserve" Uint.decoder
        |> Pipeline.required "ccollateralReserve" Uint.decoder
        |> Pipeline.required "totalLiquidity" Uint.decoder
        |> Pipeline.required "totalBond" Uint.decoder
        |> Pipeline.required "totalInsurance" Uint.decoder
        |> Pipeline.required "totalDebtCreated" Uint.decoder
        |> Pipeline.required "spot" (Uint.decoder |> Decode.nullable)


encode : PoolInfo -> Value
encode poolInfo =
    [ ( "x", poolInfo.x |> Uint.encode )
    , ( "y", poolInfo.y |> Uint.encode )
    , ( "z", poolInfo.z |> Uint.encode )
    , ( "assetReserve"
      , poolInfo.assetReserve |> Uint.encode
      )
    , ( "collateralReserve"
      , poolInfo.collateralReserve |> Uint.encode
      )
    , ( "totalLiquidity"
      , poolInfo.totalLiquidity |> Uint.encode
      )
    , ( "totalBond"
      , poolInfo.totalBond |> Uint.encode
      )
    , ( "totalInsurance"
      , poolInfo.totalInsurance |> Uint.encode
      )
    , ( "totalDebtCreated"
      , poolInfo.totalDebtCreated |> Uint.encode
      )
    , ( "spot"
      , poolInfo.spot
            |> Maybe.map Uint.encode
            |> Maybe.withDefault Encode.null
      )
    ]
        |> Encode.object
