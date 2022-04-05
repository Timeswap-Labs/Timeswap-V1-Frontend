module Page.Transaction.Borrow.Borrow.Query exposing
    ( Answer(..)
    , ResultCollateral
    , ResultDebt
    , ResultMax
    , ResultPercent
    , decoder
    , givenCollateral
    , givenDebt
    , givenMax
    , givenPercent
    )

import Data.CDP as CDP exposing (CDP)
import Data.Chain as Chain exposing (Chain)
import Data.Percent as Percent exposing (Percent)
import Data.Pool as Pool exposing (Pool)
import Data.Slippage as Slippage exposing (Slippage)
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Page.PoolInfo as PoolInfo exposing (PoolInfo)
import Page.Transaction.Borrow.Borrow.Error as Error exposing (Error)


type alias QueryPercent =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetOut : Uint
    , percent : Percent
    , slippage : Slippage
    }


type alias QueryMax =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , collateralIn : Uint
    , percent : Percent
    , slippage : Slippage
    }


type alias QueryDebt =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetOut : Uint
    , debtIn : Uint
    , slippage : Slippage
    }


type alias QueryCollateral =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetOut : Uint
    , collateralIn : Uint
    , slippage : Slippage
    }


type Answer
    = GivenPercent AnswerPercent
    | GivenMax AnswerMax
    | GivenDebt AnswerDebt
    | GivenCollateral AnswerCollateral


type alias AnswerPercent =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetOut : Uint
    , percent : Percent
    , slippage : Slippage
    , result : Result Error ResultPercent
    }


type alias AnswerMax =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , collateralIn : Uint
    , percent : Percent
    , slippage : Slippage
    , result : Result Error ResultMax
    }


type alias AnswerDebt =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetOut : Uint
    , debtIn : Uint
    , slippage : Slippage
    , result : Result Error ResultDebt
    }


type alias AnswerCollateral =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetOut : Uint
    , collateralIn : Uint
    , slippage : Slippage
    , result : Result Error ResultCollateral
    }


type alias ResultPercent =
    { debtIn : Uint
    , collateralIn : Uint
    , maxDebt : Uint
    , maxCollateral : Uint
    , apr : Float
    , cdp : CDP
    , futureApr : Float
    , futureCdp : CDP
    , txnFee : Uint
    }


type alias ResultMax =
    { assetOut : Uint
    , debtIn : Uint
    , maxDebt : Uint
    , apr : Float
    , cdp : CDP
    , futureApr : Float
    , futureCdp : CDP
    }


type alias ResultDebt =
    { percent : Percent
    , collateralIn : Uint
    , maxCollateral : Uint
    , apr : Float
    , cdp : CDP
    , futureApr : Float
    , futureCdp : CDP
    , txnFee : Uint
    }


type alias ResultCollateral =
    { percent : Percent
    , debtIn : Uint
    , maxDebt : Uint
    , apr : Float
    , cdp : CDP
    , futureApr : Float
    , futureCdp : CDP
    , txnFee : Uint
    }


givenPercent : QueryPercent -> Value
givenPercent { chain, pool, poolInfo, assetOut, percent, slippage } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "assetOut", assetOut |> Uint.encode )
    , ( "percent", percent |> Percent.encode )
    , ( "slippage", slippage |> Slippage.encodeGivenPercent )
    ]
        |> Encode.object


givenMax : QueryMax -> Value
givenMax { chain, pool, poolInfo, collateralIn, percent, slippage } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "collateralIn", collateralIn |> Uint.encode )
    , ( "percent", percent |> Percent.encode )
    , ( "slippage", slippage |> Slippage.encodeGivenPercent )
    ]
        |> Encode.object


givenDebt : QueryDebt -> Value
givenDebt { chain, pool, poolInfo, assetOut, debtIn, slippage } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "assetOut", assetOut |> Uint.encode )
    , ( "debtIn", debtIn |> Uint.encode )
    , ( "slippage", slippage |> Slippage.encode )
    ]
        |> Encode.object


givenCollateral : QueryCollateral -> Value
givenCollateral { chain, pool, poolInfo, assetOut, collateralIn, slippage } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "assetOut", assetOut |> Uint.encode )
    , ( "collateralIn", collateralIn |> Uint.encode )
    , ( "slippage", slippage |> Slippage.encode )
    ]
        |> Encode.object


decoder : Decoder Answer
decoder =
    [ decoderAnswerPercent
        |> Decode.map GivenPercent
    , decoderAnswerMax
        |> Decode.map GivenMax
    , decoderAnswerDebt
        |> Decode.map GivenDebt
    , decoderAnswerCollateral
        |> Decode.map GivenCollateral
    ]
        |> Decode.oneOf


decoderAnswerPercent : Decoder AnswerPercent
decoderAnswerPercent =
    Decode.succeed AnswerPercent
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "poolInfo" PoolInfo.decoder
        |> Pipeline.required "assetOut" Uint.decoder
        |> Pipeline.required "percent" Percent.decoder
        |> Pipeline.required "slippage" Slippage.decoderGivenPercent
        |> Pipeline.required "result"
            ([ decoderResultPercent |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )


decoderAnswerMax : Decoder AnswerMax
decoderAnswerMax =
    Decode.succeed AnswerMax
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "poolInfo" PoolInfo.decoder
        |> Pipeline.required "collateralIn" Uint.decoder
        |> Pipeline.required "percent" Percent.decoder
        |> Pipeline.required "slippage" Slippage.decoder
        |> Pipeline.required "result"
            ([ decoderResultMax |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )


decoderAnswerDebt : Decoder AnswerDebt
decoderAnswerDebt =
    Decode.succeed AnswerDebt
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "poolInfo" PoolInfo.decoder
        |> Pipeline.required "assetOut" Uint.decoder
        |> Pipeline.required "debtIn" Uint.decoder
        |> Pipeline.required "slippage" Slippage.decoder
        |> Pipeline.required "result"
            ([ decoderResultDebt |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )


decoderAnswerCollateral : Decoder AnswerCollateral
decoderAnswerCollateral =
    Decode.succeed AnswerCollateral
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "poolInfo" PoolInfo.decoder
        |> Pipeline.required "assetOut" Uint.decoder
        |> Pipeline.required "collateralIn" Uint.decoder
        |> Pipeline.required "slippage" Slippage.decoder
        |> Pipeline.required "result"
            ([ decoderResultCollateral |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )


decoderResultPercent : Decoder ResultPercent
decoderResultPercent =
    Decode.succeed ResultPercent
        |> Pipeline.required "debtIn" Uint.decoder
        |> Pipeline.required "collateralIn" Uint.decoder
        |> Pipeline.required "maxDebt" Uint.decoder
        |> Pipeline.required "maxCollateral" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder
        |> Pipeline.required "futureApr" Decode.float
        |> Pipeline.required "futureCdp" CDP.decoder
        |> Pipeline.required "txnFee" Uint.decoder


decoderResultMax : Decoder ResultMax
decoderResultMax =
    Decode.succeed ResultMax
        |> Pipeline.required "assetOut" Uint.decoder
        |> Pipeline.required "debtIn" Uint.decoder
        |> Pipeline.required "maxDebt" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder
        |> Pipeline.required "futureApr" Decode.float
        |> Pipeline.required "futureCdp" CDP.decoder


decoderResultDebt : Decoder ResultDebt
decoderResultDebt =
    Decode.succeed ResultDebt
        |> Pipeline.required "percent" Percent.decoder
        |> Pipeline.required "collateralIn" Uint.decoder
        |> Pipeline.required "maxCollateral" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder
        |> Pipeline.required "futureApr" Decode.float
        |> Pipeline.required "futureCdp" CDP.decoder
        |> Pipeline.required "txnFee" Uint.decoder


decoderResultCollateral : Decoder ResultCollateral
decoderResultCollateral =
    Decode.succeed ResultCollateral
        |> Pipeline.required "percent" Percent.decoder
        |> Pipeline.required "debtIn" Uint.decoder
        |> Pipeline.required "maxDebt" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder
        |> Pipeline.required "futureApr" Decode.float
        |> Pipeline.required "futureCdp" CDP.decoder
        |> Pipeline.required "txnFee" Uint.decoder
