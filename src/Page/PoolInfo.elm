module Page.PoolInfo exposing (PoolInfo, decoder, dummy, encode)

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
    , assetSpot : Maybe Float
    , collateralSpot : Maybe Float
    }


dummy : PoolInfo
dummy =
    { x = Uint.dummy
    , y = Uint.dummy
    , z = Uint.dummy
    , assetReserve = Uint.dummy
    , collateralReserve = Uint.dummy
    , totalLiquidity = Uint.dummy
    , totalBond = Uint.dummy
    , totalInsurance = Uint.dummy
    , totalDebtCreated = Uint.dummy
    , assetSpot = Nothing
    , collateralSpot = Nothing
    }


decoder : Decoder PoolInfo
decoder =
    Decode.succeed PoolInfo
        |> Pipeline.required "x" Uint.decoder
        |> Pipeline.required "y" Uint.decoder
        |> Pipeline.required "z" Uint.decoder
        |> Pipeline.required "assetReserve" Uint.decoder
        |> Pipeline.required "collateralReserve" Uint.decoder
        |> Pipeline.required "totalLiquidity" Uint.decoder
        |> Pipeline.required "totalBond" Uint.decoder
        |> Pipeline.required "totalInsurance" Uint.decoder
        |> Pipeline.required "totalDebtCreated" Uint.decoder
        |> Pipeline.required "assetSpot" (Decode.float |> Decode.nullable)
        |> Pipeline.required "collateralSpot" (Decode.float |> Decode.nullable)


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
    , ( "assetSpot"
      , poolInfo.assetSpot
            |> Maybe.map Encode.float
            |> Maybe.withDefault Encode.null
      )
    , ( "collateralSpot"
      , poolInfo.collateralSpot
            |> Maybe.map Encode.float
            |> Maybe.withDefault Encode.null
      )
    ]
        |> Encode.object
