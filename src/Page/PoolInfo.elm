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
    , totalBondInterest : Uint
    , totalBondPrincipal : Uint
    , totalInsuranceInterest : Uint
    , totalInsurancePrincipal : Uint
    , totalDebtCreated : Uint
    , assetSpot : Maybe Float
    , collateralSpot : Maybe Float
    , fee : Int
    , protocolFee : Int
    }


dummy : PoolInfo
dummy =
    { x = Uint.dummy
    , y = Uint.dummy
    , z = Uint.dummy
    , assetReserve = Uint.dummy
    , collateralReserve = Uint.dummy
    , totalLiquidity = Uint.dummy
    , totalBondInterest = Uint.dummy
    , totalBondPrincipal = Uint.dummy
    , totalInsuranceInterest = Uint.dummy
    , totalInsurancePrincipal = Uint.dummy
    , totalDebtCreated = Uint.dummy
    , assetSpot = Nothing
    , collateralSpot = Nothing
    , fee = 196
    , protocolFee = 17
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
        |> Pipeline.required "totalBondInterest" Uint.decoder
        |> Pipeline.required "totalBondPrincipal" Uint.decoder
        |> Pipeline.required "totalInsuranceInterest" Uint.decoder
        |> Pipeline.required "totalInsurancePrincipal" Uint.decoder
        |> Pipeline.required "totalDebtCreated" Uint.decoder
        |> Pipeline.required "assetSpot" (Decode.float |> Decode.nullable)
        |> Pipeline.required "collateralSpot" (Decode.float |> Decode.nullable)
        |> Pipeline.required "fee" Decode.int
        |> Pipeline.required "protocolFee" Decode.int


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
    , ( "totalBondInterest"
      , poolInfo.totalBondInterest |> Uint.encode
      )
    , ( "totalBondPrincipal"
      , poolInfo.totalBondPrincipal |> Uint.encode
      )
    , ( "totalInsuranceInterest"
      , poolInfo.totalInsuranceInterest |> Uint.encode
      )
    , ( "totalInsurancePrincipal"
      , poolInfo.totalInsurancePrincipal |> Uint.encode
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
    , ( "fee"
      , poolInfo.fee |> Encode.int
      )
    , ( "protocolFee"
      , poolInfo.protocolFee |> Encode.int
      )
    ]
        |> Encode.object
