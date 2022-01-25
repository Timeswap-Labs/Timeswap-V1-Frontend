module Page.Transaction.Liquidity.Add.Query exposing
    ( Answer(..)
    , ResultAsset
    , ResultCollateral
    , ResultDebt
    , decoder
    , givenAsset
    , givenCollateral
    , givenDebt
    )

import Data.CDP as CDP exposing (CDP)
import Data.Chain as Chain exposing (Chain)
import Data.Pool as Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Slippage as Slippage exposing (Slippage)
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Page.PoolInfo as PoolInfo exposing (PoolInfo)
import Page.Transaction.Liquidity.Add.Error as Error exposing (Error)


type alias QueryAsset =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetIn : Uint
    , slippage : Slippage
    }


type alias QueryDebt =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , debtIn : Uint
    , slippage : Slippage
    }


type alias QueryCollateral =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , collateralIn : Uint
    , slippage : Slippage
    }


type Answer
    = GivenAsset AnswerAsset
    | GivenDebt AnswerDebt
    | GivenCollateral AnswerCollateral


type alias AnswerAsset =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetIn : Uint
    , slippage : Slippage
    , result : Result Error ResultAsset
    }


type alias AnswerDebt =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , debtIn : Uint
    , slippage : Slippage
    , result : Result Error ResultDebt
    }


type alias AnswerCollateral =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , collateralIn : Uint
    , slippage : Slippage
    , result : Result Error ResultCollateral
    }


type alias ResultAsset =
    { debtIn : Uint
    , collateralIn : Uint
    , liquidityOut : Uint
    , minLiquidity : Uint
    , maxDebt : Uint
    , maxCollateral : Uint
    , apr : Float
    , cdp : CDP
    }


type alias ResultDebt =
    { assetIn : Uint
    , collateralIn : Uint
    , liquidityOut : Uint
    , minLiquidity : Uint
    , maxAsset : Uint
    , maxCollateral : Uint
    , apr : Float
    , cdp : CDP
    }


type alias ResultCollateral =
    { assetIn : Uint
    , debtIn : Uint
    , liquidityOut : Uint
    , minLiquidity : Uint
    , maxAsset : Uint
    , maxDebt : Uint
    , apr : Float
    , cdp : CDP
    }


givenAsset : QueryAsset -> Value
givenAsset { chain, pool, poolInfo, assetIn, slippage } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "slippage", slippage |> Slippage.encodeGivenPercent )
    ]
        |> Encode.object


givenDebt : QueryDebt -> Value
givenDebt { chain, pool, poolInfo, debtIn, slippage } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "debtIn", debtIn |> Uint.encode )
    , ( "slippage", slippage |> Slippage.encodeGivenPercent )
    ]
        |> Encode.object


givenCollateral : QueryCollateral -> Value
givenCollateral { chain, pool, poolInfo, collateralIn, slippage } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "collateralIn", collateralIn |> Uint.encode )
    , ( "slippage", slippage |> Slippage.encodeGivenPercent )
    ]
        |> Encode.object


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
        |> Pipeline.required "chain" Chain.decoder
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
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "poolInfo" PoolInfo.decoder
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
        |> Pipeline.required "collateralIn" Uint.decoder
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
        |> Pipeline.required "debtIn" Uint.decoder
        |> Pipeline.required "collateralIn" Uint.decoder
        |> Pipeline.required "liquidityOut" Uint.decoder
        |> Pipeline.required "minLiquidity" Uint.decoder
        |> Pipeline.required "maxDebt" Uint.decoder
        |> Pipeline.required "maxCollateral" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder


decoderResultDebt : Decoder ResultDebt
decoderResultDebt =
    Decode.succeed ResultDebt
        |> Pipeline.required "assetIn" Uint.decoder
        |> Pipeline.required "collateralIn" Uint.decoder
        |> Pipeline.required "liquidityOut" Uint.decoder
        |> Pipeline.required "minLiquidity" Uint.decoder
        |> Pipeline.required "maxAsset" Uint.decoder
        |> Pipeline.required "maxCollateral" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder


decoderResultCollateral : Decoder ResultCollateral
decoderResultCollateral =
    Decode.succeed ResultCollateral
        |> Pipeline.required "assetIn" Uint.decoder
        |> Pipeline.required "debtIn" Uint.decoder
        |> Pipeline.required "liquidityOut" Uint.decoder
        |> Pipeline.required "minLiquidity" Uint.decoder
        |> Pipeline.required "maxAsset" Uint.decoder
        |> Pipeline.required "maxDebt" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder
