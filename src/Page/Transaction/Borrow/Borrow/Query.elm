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
import Page.Transaction.Borrow.Borrow.Error as Error exposing (Error)
import Page.Transaction.PoolInfo as PoolInfo exposing (PoolInfo)


type alias QueryPercent =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetOut : Uint
    , percent : Percent
    , slippage : Slippage
    }


type alias QueryMax =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , collateralIn : Uint
    , percent : Percent
    , slippage : Slippage
    }


type alias QueryDebt =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetOut : Uint
    , debtIn : Uint
    , slippage : Slippage
    }


type alias QueryCollateral =
    { chainId : Chain
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
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetOut : Uint
    , percent : Percent
    , slippage : Slippage
    , result : Result Error ResultPercent
    }


type alias AnswerMax =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , collateralIn : Uint
    , percent : Percent
    , slippage : Slippage
    , result : Result Error ResultMax
    }


type alias AnswerDebt =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetOut : Uint
    , debtIn : Uint
    , slippage : Slippage
    , result : Result Error ResultDebt
    }


type alias AnswerCollateral =
    { chainId : Chain
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
    }


type alias ResultMax =
    { assetOut : Uint
    , debtIn : Uint
    , maxDebt : Uint
    , apr : Float
    , cdp : CDP
    }


type alias ResultDebt =
    { percent : Percent
    , collateralIn : Uint
    , maxCollateral : Uint
    , apr : Float
    , cdp : CDP
    }


type alias ResultCollateral =
    { percent : Percent
    , debtIn : Uint
    , maxDebt : Uint
    , apr : Float
    , cdp : CDP
    }


givenPercent : QueryPercent -> Value
givenPercent { chainId, pool, poolInfo, assetOut, percent, slippage } =
    [ ( "chainId", chainId |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "assetOut", assetOut |> Uint.encode )
    , ( "percent", percent |> Percent.encode )
    , ( "slippage", slippage |> Slippage.encodeGivenPercent )
    ]
        |> Encode.object


givenMax : QueryMax -> Value
givenMax { chainId, pool, poolInfo, collateralIn, percent, slippage } =
    [ ( "chainId", chainId |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "collateralIn", collateralIn |> Uint.encode )
    , ( "percent", percent |> Percent.encode )
    , ( "slippage", slippage |> Slippage.encodeGivenPercent )
    ]
        |> Encode.object


givenDebt : QueryDebt -> Value
givenDebt { chainId, pool, poolInfo, assetOut, debtIn, slippage } =
    [ ( "chainId", chainId |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "assetOut", assetOut |> Uint.encode )
    , ( "debtIn", debtIn |> Uint.encode )
    , ( "slippage", slippage |> Slippage.encode )
    ]
        |> Encode.object


givenCollateral : QueryCollateral -> Value
givenCollateral { chainId, pool, poolInfo, assetOut, collateralIn, slippage } =
    [ ( "chainId", chainId |> Chain.encode )
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
        |> Pipeline.required "chainId" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "poolInfo" PoolInfo.decoder
        |> Pipeline.required "assetOut" Uint.decoder
        |> Pipeline.required "percent" Percent.decoder
        |> Pipeline.required "slippage" Slippage.decoder
        |> Pipeline.required "result"
            ([ decoderResultPercent |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )


decoderAnswerMax : Decoder AnswerMax
decoderAnswerMax =
    Decode.succeed AnswerMax
        |> Pipeline.required "chainId" Chain.decoder
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
        |> Pipeline.required "chainId" Chain.decoder
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
        |> Pipeline.required "chainId" Chain.decoder
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


decoderResultMax : Decoder ResultMax
decoderResultMax =
    Decode.succeed ResultMax
        |> Pipeline.required "assetOut" Uint.decoder
        |> Pipeline.required "debtIn" Uint.decoder
        |> Pipeline.required "maxDebt" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder


decoderResultDebt : Decoder ResultDebt
decoderResultDebt =
    Decode.succeed ResultDebt
        |> Pipeline.required "percent" Percent.decoder
        |> Pipeline.required "collateralIn" Uint.decoder
        |> Pipeline.required "maxCollateral" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder


decoderResultCollateral : Decoder ResultCollateral
decoderResultCollateral =
    Decode.succeed ResultCollateral
        |> Pipeline.required "percent" Percent.decoder
        |> Pipeline.required "debtIn" Uint.decoder
        |> Pipeline.required "maxDebt" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder
