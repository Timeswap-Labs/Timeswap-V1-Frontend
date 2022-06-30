module Page.Position.Liq.Query exposing
    ( ActiveReturn
    , Answer
    , Query
    , decoder
    , givenLiq
    )

import Blockchain.User.Liq as Liq exposing (Liq)
import Blockchain.User.Return as Return exposing (Return)
import Blockchain.User.TokenId as TokenId exposing (TokenId)
import Data.Address as Address exposing (Address)
import Data.Chain as Chain exposing (Chain)
import Data.Maturity as Maturity
import Data.Pool as Pool exposing (Pool)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Page.PoolInfo as PoolInfo exposing (PoolInfo)
import Page.Position.Liq.Error as Error exposing (Error)


type alias Query =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , liquidityIn : Liq
    , tokenIds : List TokenId
    , cdtAddress : Maybe Address
    }


type alias Answer =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , liquidityInt : Liq
    , result : Result Error (Maturity.Status Return ActiveReturn)
    }


type alias ActiveReturn =
    { liqPercent : Float
    , isFlashRepayAllowed : Bool
    , isCDTApproved : Bool
    }


givenLiq : Query -> Value
givenLiq { chain, pool, poolInfo, liquidityIn, tokenIds, cdtAddress } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "liquidityIn", liquidityIn |> Liq.encode )
    , ( "tokenIds", tokenIds |> Encode.list TokenId.encode )
    , ( "cdtAddress"
      , case cdtAddress of
            Just address ->
                address |> Address.encode

            _ ->
                Encode.null
      )
    ]
        |> Encode.object


decoder : Decoder Answer
decoder =
    Decode.succeed Answer
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "poolInfo" PoolInfo.decoder
        |> Pipeline.required "liquidityIn" Liq.decoder
        |> Pipeline.required "result"
            ([ activeReturnDecoder
                |> Decode.map Maturity.Active
                |> Decode.map Ok
             , Return.decoder
                |> Decode.map Maturity.Matured
                |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )


activeReturnDecoder : Decoder ActiveReturn
activeReturnDecoder =
    Decode.succeed ActiveReturn
        |> Pipeline.required "liqPercent" Decode.float
        |> Pipeline.required "isFlashRepayAllowed" Decode.bool
        |> Pipeline.required "isCDTApproved" Decode.bool
