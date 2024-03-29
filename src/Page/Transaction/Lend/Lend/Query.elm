module Page.Transaction.Lend.Lend.Query exposing
    ( Answer(..)
    , ResultBond
    , ResultInsurance
    , ResultPercent
    , decoder
    , givenBond
    , givenInsurance
    , givenPercent
    )

import Data.CDP as CDP exposing (CDP)
import Data.Chain as Chain exposing (Chain)
import Data.Percent as Percent exposing (Percent)
import Data.Pool as Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Slippage as Slippage exposing (Slippage)
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Page.PoolInfo as PoolInfo exposing (PoolInfo)
import Page.Transaction.Lend.Lend.Error as Error exposing (Error)


type alias QueryPercent =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetIn : Uint
    , percent : Percent
    , slippage : Slippage
    }


type alias QueryBond =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetIn : Uint
    , bondOut : Uint
    , slippage : Slippage
    }


type alias QueryInsurance =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetIn : Uint
    , insuranceOut : Uint
    , slippage : Slippage
    }


type Answer
    = GivenPercent AnswerPercent
    | GivenBond AnswerBond
    | GivenInsurance AnswerInsurance


type alias AnswerPercent =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetIn : Uint
    , percent : Percent
    , slippage : Slippage
    , result : Result Error ResultPercent
    }


type alias AnswerBond =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetIn : Uint
    , bondOut : Uint
    , slippage : Slippage
    , result : Result Error ResultBond
    }


type alias AnswerInsurance =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetIn : Uint
    , insuranceOut : Uint
    , slippage : Slippage
    , result : Result Error ResultInsurance
    }


type alias ResultPercent =
    { bondOut : Uint
    , insuranceOut : Uint
    , minBond : Uint
    , minInsurance : Uint
    , apr : Float
    , cdp : CDP
    , futureApr : Float
    , futureCdp : CDP
    , txnFee : Uint
    }


type alias ResultBond =
    { percent : Percent
    , insuranceOut : Uint
    , minInsurance : Uint
    , apr : Float
    , cdp : CDP
    , futureApr : Float
    , futureCdp : CDP
    , txnFee : Uint
    }


type alias ResultInsurance =
    { percent : Percent
    , bondOut : Uint
    , minBond : Uint
    , apr : Float
    , cdp : CDP
    , futureApr : Float
    , futureCdp : CDP
    , txnFee : Uint
    }


givenPercent : QueryPercent -> Value
givenPercent { chain, pool, poolInfo, assetIn, percent, slippage } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "percent", percent |> Percent.encode )
    , ( "slippage", slippage |> Slippage.encodeGivenPercent )
    ]
        |> Encode.object


givenBond : QueryBond -> Value
givenBond { chain, pool, poolInfo, assetIn, bondOut, slippage } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "bondOut", bondOut |> Uint.encode )
    , ( "slippage", slippage |> Slippage.encode )
    ]
        |> Encode.object


givenInsurance : QueryInsurance -> Value
givenInsurance { chain, pool, poolInfo, assetIn, insuranceOut, slippage } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "insuranceOut", insuranceOut |> Uint.encode )
    , ( "slippage", slippage |> Slippage.encode )
    ]
        |> Encode.object


decoder : Decoder Answer
decoder =
    [ decoderAnswerPercent
        |> Decode.map GivenPercent
    , decoderAnswerBond
        |> Decode.map GivenBond
    , decoderAnswerInsurance
        |> Decode.map GivenInsurance
    ]
        |> Decode.oneOf


decoderAnswerPercent : Decoder AnswerPercent
decoderAnswerPercent =
    Decode.succeed AnswerPercent
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "poolInfo" PoolInfo.decoder
        |> Pipeline.required "assetIn" Uint.decoder
        |> Pipeline.required "percent" Percent.decoder
        |> Pipeline.required "slippage" Slippage.decoderGivenPercent
        |> Pipeline.required "result"
            ([ decoderResultPercent |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )


decoderAnswerBond : Decoder AnswerBond
decoderAnswerBond =
    Decode.succeed AnswerBond
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "poolInfo" PoolInfo.decoder
        |> Pipeline.required "assetIn" Uint.decoder
        |> Pipeline.required "bondOut" Uint.decoder
        |> Pipeline.required "slippage" Slippage.decoder
        |> Pipeline.required "result"
            ([ decoderResultBond |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )


decoderAnswerInsurance : Decoder AnswerInsurance
decoderAnswerInsurance =
    Decode.succeed AnswerInsurance
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "poolInfo" PoolInfo.decoder
        |> Pipeline.required "assetIn" Uint.decoder
        |> Pipeline.required "insuranceOut" Uint.decoder
        |> Pipeline.required "slippage" Slippage.decoder
        |> Pipeline.required "result"
            ([ decoderResultInsurance |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )


decoderResultPercent : Decoder ResultPercent
decoderResultPercent =
    Decode.succeed ResultPercent
        |> Pipeline.required "bondOut" Uint.decoder
        |> Pipeline.required "insuranceOut" Uint.decoder
        |> Pipeline.required "minBond" Uint.decoder
        |> Pipeline.required "minInsurance" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder
        |> Pipeline.required "futureApr" Decode.float
        |> Pipeline.required "futureCdp" CDP.decoder
        |> Pipeline.required "txnFee" Uint.decoder


decoderResultBond : Decoder ResultBond
decoderResultBond =
    Decode.succeed ResultBond
        |> Pipeline.required "percent" Percent.decoder
        |> Pipeline.required "insuranceOut" Uint.decoder
        |> Pipeline.required "minInsurance" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder
        |> Pipeline.required "futureApr" Decode.float
        |> Pipeline.required "futureCdp" CDP.decoder
        |> Pipeline.required "txnFee" Uint.decoder


decoderResultInsurance : Decoder ResultInsurance
decoderResultInsurance =
    Decode.succeed ResultInsurance
        |> Pipeline.required "percent" Percent.decoder
        |> Pipeline.required "bondOut" Uint.decoder
        |> Pipeline.required "minBond" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder
        |> Pipeline.required "futureApr" Decode.float
        |> Pipeline.required "futureCdp" CDP.decoder
        |> Pipeline.required "txnFee" Uint.decoder
