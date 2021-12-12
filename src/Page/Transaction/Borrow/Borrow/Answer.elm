module Page.Transaction.Borrow.Borrow.Answer exposing
    ( Answer(..)
    , ResultCollateral
    , ResultDebt
    , ResultMax
    , ResultPercent
    , decoder
    )

import Data.CDP as CDP exposing (CDP)
import Data.Chain as Chain exposing (Chain)
import Data.Percent as Percent exposing (Percent)
import Data.Pool as Pool exposing (Pool)
import Data.Slippage as Slippage exposing (Slippage)
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Page.Transaction.Borrow.Borrow.Error as Error exposing (Error)
import Page.Transaction.PoolInfo as PoolInfo exposing (PoolInfo)


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
    , collateralOut : Uint
    , percent : Percent
    , slippage : Slippage
    , result : Result Error ResultMax
    }


type alias AnswerDebt =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetOut : Uint
    , debtOut : Uint
    , slippage : Slippage
    , result : Result Error ResultDebt
    }


type alias AnswerCollateral =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetOut : Uint
    , collateralOut : Uint
    , slippage : Slippage
    , result : Result Error ResultCollateral
    }


type alias ResultPercent =
    { debtOut : Uint
    , collateralOut : Uint
    , maxDebt : Uint
    , maxCollateral : Uint
    , apr : Float
    , cdp : CDP
    }


type alias ResultMax =
    { assetOut : Uint
    , debtOut : Uint
    , maxDebt : Uint
    , apr : Float
    , cdp : CDP
    }


type alias ResultDebt =
    { percent : Percent
    , collateralOut : Uint
    , maxCollateral : Uint
    , apr : Float
    , cdp : CDP
    }


type alias ResultCollateral =
    { percent : Percent
    , debtOut : Uint
    , maxDebt : Uint
    , apr : Float
    , cdp : CDP
    }


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
        |> Pipeline.required "collateralOut" Uint.decoder
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
        |> Pipeline.required "debtOut" Uint.decoder
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
        |> Pipeline.required "collateralOut" Uint.decoder
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
        |> Pipeline.required "debtOut" Uint.decoder
        |> Pipeline.required "collateralOut" Uint.decoder
        |> Pipeline.required "maxDebt" Uint.decoder
        |> Pipeline.required "maxCollateral" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder


decoderResultMax : Decoder ResultMax
decoderResultMax =
    Decode.succeed ResultMax
        |> Pipeline.required "assetOut" Uint.decoder
        |> Pipeline.required "debtOut" Uint.decoder
        |> Pipeline.required "maxDebt" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder


decoderResultDebt : Decoder ResultDebt
decoderResultDebt =
    Decode.succeed ResultDebt
        |> Pipeline.required "percent" Percent.decoder
        |> Pipeline.required "collateralOut" Uint.decoder
        |> Pipeline.required "maxCollateral" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder


decoderResultCollateral : Decoder ResultCollateral
decoderResultCollateral =
    Decode.succeed ResultCollateral
        |> Pipeline.required "percent" Percent.decoder
        |> Pipeline.required "debtOut" Uint.decoder
        |> Pipeline.required "maxDebt" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder
