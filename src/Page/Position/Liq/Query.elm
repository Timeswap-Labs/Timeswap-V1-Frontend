module Page.Position.Liq.Query exposing
    ( ActiveReturn
    , Answer
    , FlashRepayAnswer
    , FlashRepayData
    , FlashRepayQuery
    , Query
    , decoder
    , encodeFlashRepayQuery
    , encodeFlashRepayTry
    , flashRepayDecoder
    , flashRepayTryDecoder
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
    }


type alias FlashRepayQuery =
    { chain : Chain
    , pool : Pool
    , tokenIds : List TokenId
    , cdtAddress : Address
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
    }


type alias FlashRepayAnswer =
    { chain : Chain
    , pool : Pool
    , result : Result Error FlashRepayData
    }


type alias FlashRepayTryAnswer =
    { chain : Chain
    , pool : Pool
    , tokenIds : List TokenId
    , result : Result Error Bool
    }


type alias FlashRepayData =
    { isCDTApproved : Bool
    , liqDueTokenIds : List TokenId
    }


givenLiq : Query -> Value
givenLiq { chain, pool, poolInfo, liquidityIn } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "liquidityIn", liquidityIn |> Liq.encode )
    ]
        |> Encode.object


encodeFlashRepayQuery : FlashRepayQuery -> Value
encodeFlashRepayQuery { chain, pool, tokenIds, cdtAddress } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "tokenIds", tokenIds |> Encode.list TokenId.encode )
    , ( "cdtAddress", cdtAddress |> Address.encode )
    ]
        |> Encode.object


encodeFlashRepayTry :
    { chain : Chain
    , pool : Pool
    , tokenIds : List TokenId
    }
    -> Value
encodeFlashRepayTry { chain, pool, tokenIds } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "tokenIds", tokenIds |> Encode.list TokenId.encode )
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


flashRepayDecoder : Decoder FlashRepayAnswer
flashRepayDecoder =
    Decode.succeed FlashRepayAnswer
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "result"
            ([ flashRepayDataDecoder
                |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )


flashRepayDataDecoder : Decoder FlashRepayData
flashRepayDataDecoder =
    Decode.succeed FlashRepayData
        |> Pipeline.required "isCDTApproved" Decode.bool
        |> Pipeline.required "liqDueTokenIds" (Decode.list TokenId.decoder)


flashRepayTryDecoder : Decoder FlashRepayTryAnswer
flashRepayTryDecoder =
    Decode.succeed FlashRepayTryAnswer
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "tokenIds" (Decode.list TokenId.decoder)
        |> Pipeline.required "result"
            ([ Decode.bool
                |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )
