module Page.Transaction.Liquidity.Add.Answer exposing
    ( Answer(..)
    , ResultAsset
    , ResultCollateral
    , ResultDebt
    , decoder
    , decoderResultCollateral
    , decoderResultDebt
    )

import Data.CDP as CDP exposing (CDP)
import Data.Chain as Chain exposing (Chain)
import Data.Pool as Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Slippage as Slippage exposing (Slippage)
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Page.Transaction.Liquidity.Add.Error as Error exposing (Error)
import Page.Transaction.PoolInfo as PoolInfo exposing (PoolInfo)


type Answer
    = GivenAsset AnswerAsset
    | GivenDebt AnswerDebt
    | GivenCollateral AnswerCollateral


type alias AnswerAsset =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetIn : Uint
    , slippage : Slippage
    , result : Result Error ResultAsset
    }


type alias AnswerDebt =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , debtOut : Uint
    , slippage : Slippage
    , result : Result Error ResultDebt
    }


type alias AnswerCollateral =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , collateralOut : Uint
    , slippage : Slippage
    , result : Result Error ResultCollateral
    }


type alias ResultAsset =
    { debtOut : Uint
    , collateralOut : Uint
    , liquidityOut : Uint
    , maxDebt : Uint
    , maxCollateral : Uint
    , minLiquidity : Uint
    , apr : Float
    , cdp : CDP
    }


type alias ResultDebt =
    { assetIn : Uint
    , collateralOut : Uint
    , liquidityOut : Uint
    , maxDebt : Uint
    , maxCollateral : Uint
    , minLiquidity : Uint
    , apr : Float
    , cdp : CDP
    }


type alias ResultCollateral =
    { assetIn : Uint
    , debtOut : Uint
    , liquidityOut : Uint
    , maxAsset : Uint
    , maxDebt : Uint
    , minLiquidity : Uint
    , apr : Float
    , cdp : CDP
    }


decoder : Decoder Answer
decoder =
    [ decoderAnswerAsset
        |> Decode.map GivenAsset
    , decoderAnswerDebt
        |> Decode.map GivenDebt
    , decoderAnswerCollateral
        |> Decode.map GivenCollateral
    ]
        |> Decode.oneOf


decoderAnswerAsset : Decoder AnswerAsset
decoderAnswerAsset =
    Decode.succeed AnswerAsset
        |> Pipeline.required "chainId" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "poolInfo" PoolInfo.decoder
        |> Pipeline.required "assetIn" Uint.decoder
        |> Pipeline.required "slippage" Slippage.decoder
        |> Pipeline.required "result"
            ([ decoderResultAsset |> Decode.map Ok
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
        |> Pipeline.required "collateralOut" Uint.decoder
        |> Pipeline.required "slippage" Slippage.decoder
        |> Pipeline.required "result"
            ([ decoderResultCollateral |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )


decoderResultAsset : Decoder ResultAsset
decoderResultAsset =
    Decode.succeed ResultAsset
        |> Pipeline.required "debtOut" Uint.decoder
        |> Pipeline.required "collateralOut" Uint.decoder
        |> Pipeline.required "liquidityOut" Uint.decoder
        |> Pipeline.required "maxDebt" Uint.decoder
        |> Pipeline.required "maxCollateral" Uint.decoder
        |> Pipeline.required "minLiquidity" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder


decoderResultDebt : Decoder ResultDebt
decoderResultDebt =
    Decode.succeed ResultDebt
        |> Pipeline.required "assetIn" Uint.decoder
        |> Pipeline.required "collateralOut" Uint.decoder
        |> Pipeline.required "liquidityOut" Uint.decoder
        |> Pipeline.required "maxAsset" Uint.decoder
        |> Pipeline.required "maxCollateral" Uint.decoder
        |> Pipeline.required "minLiquidity" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder


decoderResultCollateral : Decoder ResultCollateral
decoderResultCollateral =
    Decode.succeed ResultCollateral
        |> Pipeline.required "assetIn" Uint.decoder
        |> Pipeline.required "debtOut" Uint.decoder
        |> Pipeline.required "liquidityOut" Uint.decoder
        |> Pipeline.required "maxAsset" Uint.decoder
        |> Pipeline.required "maxDebt" Uint.decoder
        |> Pipeline.required "minLiquidity" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder
