module Page.PoolInfo exposing (PoolInfo, decoder, encode)

import Data.Address as Address exposing (Address)
import Data.CDP as CDP exposing (CDP)
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
    , feeStored : Uint
    , protocolFee : Int
    , apr : Float
    , cdp : CDP
    , convAddress : Address
    , totalLend : Maybe Uint
    , totalBorrow : Maybe Uint
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
        |> Pipeline.required "feeStored" Uint.decoder
        |> Pipeline.required "protocolFee" Decode.int
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder
        |> Pipeline.required "convAddress" Address.decoder
        |> Pipeline.optional "totalLend" (Uint.decoder |> Decode.nullable) Nothing
        |> Pipeline.optional "totalBorrow" (Uint.decoder |> Decode.nullable) Nothing


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
    , ( "feeStored"
      , poolInfo.feeStored |> Uint.encode
      )
    , ( "protocolFee"
      , poolInfo.protocolFee |> Encode.int
      )
    , ( "apr"
      , poolInfo.apr |> Encode.float
      )
    , ( "cdp"
      , poolInfo.cdp |> CDP.encode
      )
    , ( "convAddress"
      , poolInfo.convAddress |> Address.encode
      )
    ]
        |> Encode.object
