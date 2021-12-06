module Page.Transaction.Liquidity.Answer exposing
    ( Answer(..)
    , AnswerNew
    , LiquidityGivenNew
    , OutGivenAsset
    , OutGivenCollateral
    , OutGivenDebt
    , decoder
    , decoderCreate
    , decoderResultCollateral
    , decoderResultDebt
    , initGivenAsset
    , initGivenCollateral
    , initGivenDebt
    , initGivenNew
    , toLiquidityGivenNew
    , toOutGivenAsset
    , toOutGivenCollateral
    , toOutGivenDebt
    )

import Data.CDP as CDP exposing (CDP)
import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.Pool as Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Slippage as Slippage exposing (Slippage)
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Page.Transaction.Liquidity.Error as Error
    exposing
        ( CreateError
        , TransactionError
        )
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
    , result : Result TransactionError ResultAsset
    }


type alias AnswerDebt =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , debtOut : Uint
    , slippage : Slippage
    , result : Result TransactionError ResultDebt
    }


type alias AnswerCollateral =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , collateralOut : Uint
    , slippage : Slippage
    , result : Result TransactionError ResultCollateral
    }


type alias AnswerNew =
    { chainId : Chain
    , pool : Pool
    , assetIn : Uint
    , debtOut : Uint
    , collateralOut : Uint
    , result : Result CreateError ResultNew
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


type alias ResultNew =
    { liquidityOut : Uint
    , apr : Float
    , cdp : CDP
    }


type alias OutGivenAsset =
    ResultAsset


type alias OutGivenDebt =
    ResultDebt


type alias OutGivenCollateral =
    ResultCollateral


type alias LiquidityGivenNew =
    ResultNew


initGivenAsset : OutGivenAsset
initGivenAsset =
    { debtOut = Uint.zero
    , collateralOut = Uint.zero
    , liquidityOut = Uint.zero
    , maxDebt = Uint.zero
    , maxCollateral = Uint.zero
    , minLiquidity = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


initGivenDebt : OutGivenDebt
initGivenDebt =
    { assetIn = Uint.zero
    , collateralOut = Uint.zero
    , liquidityOut = Uint.zero
    , maxDebt = Uint.zero
    , maxCollateral = Uint.zero
    , minLiquidity = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


initGivenCollateral : OutGivenCollateral
initGivenCollateral =
    { assetIn = Uint.zero
    , debtOut = Uint.zero
    , liquidityOut = Uint.zero
    , maxAsset = Uint.zero
    , maxDebt = Uint.zero
    , minLiquidity = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


initGivenNew : LiquidityGivenNew
initGivenNew =
    { liquidityOut = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


decoder : { model | chains : Chains } -> Decoder Answer
decoder model =
    [ decoderAnswerAsset model
        |> Decode.map GivenAsset
    , decoderAnswerDebt model
        |> Decode.map GivenDebt
    , decoderAnswerCollateral model
        |> Decode.map GivenCollateral
    ]
        |> Decode.oneOf


decoderAnswerAsset :
    { model | chains : Chains }
    -> Decoder AnswerAsset
decoderAnswerAsset { chains } =
    Chains.decoderChain chains
        |> Decode.field "chainId"
        |> Decode.andThen
            (\chain ->
                Decode.succeed (AnswerAsset chain)
                    |> Pipeline.required "pool" (Pool.decoder chain chains)
                    |> Pipeline.required "poolInfo" PoolInfo.decoder
                    |> Pipeline.required "assetIn" Uint.decoder
                    |> Pipeline.required "slippage" Slippage.decoder
                    |> Pipeline.required "result"
                        ([ decoderResultAsset |> Decode.map Ok
                         , Error.decoder |> Decode.map Err
                         ]
                            |> Decode.oneOf
                        )
            )


decoderAnswerDebt :
    { model | chains : Chains }
    -> Decoder AnswerDebt
decoderAnswerDebt { chains } =
    Chains.decoderChain chains
        |> Decode.field "chainId"
        |> Decode.andThen
            (\chain ->
                Decode.succeed (AnswerDebt chain)
                    |> Pipeline.required "pool" (Pool.decoder chain chains)
                    |> Pipeline.required "poolInfo" PoolInfo.decoder
                    |> Pipeline.required "debtOut" Uint.decoder
                    |> Pipeline.required "slippage" Slippage.decoder
                    |> Pipeline.required "result"
                        ([ decoderResultDebt |> Decode.map Ok
                         , Error.decoder |> Decode.map Err
                         ]
                            |> Decode.oneOf
                        )
            )


decoderAnswerCollateral :
    { model | chains : Chains }
    -> Decoder AnswerCollateral
decoderAnswerCollateral { chains } =
    Chains.decoderChain chains
        |> Decode.field "chainId"
        |> Decode.andThen
            (\chain ->
                Decode.succeed (AnswerCollateral chain)
                    |> Pipeline.required "pool" (Pool.decoder chain chains)
                    |> Pipeline.required "poolInfo" PoolInfo.decoder
                    |> Pipeline.required "collateralOut" Uint.decoder
                    |> Pipeline.required "slippage" Slippage.decoder
                    |> Pipeline.required "result"
                        ([ decoderResultCollateral |> Decode.map Ok
                         , Error.decoder |> Decode.map Err
                         ]
                            |> Decode.oneOf
                        )
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


toOutGivenAsset :
    Result TransactionError ResultAsset
    -> Remote TransactionError OutGivenAsset
toOutGivenAsset result =
    case result of
        Ok out ->
            Success out

        Err error ->
            Failure error


toOutGivenDebt :
    Result TransactionError ResultDebt
    -> Remote TransactionError OutGivenDebt
toOutGivenDebt result =
    case result of
        Ok out ->
            Success out

        Err error ->
            Failure error


toOutGivenCollateral :
    Result TransactionError ResultCollateral
    -> Remote TransactionError OutGivenCollateral
toOutGivenCollateral result =
    case result of
        Ok out ->
            Success out

        Err error ->
            Failure error


decoderCreate : { model | chains : Chains } -> Decoder AnswerNew
decoderCreate { chains } =
    Chains.decoderChain chains
        |> Decode.field "chainId"
        |> Decode.andThen
            (\chain ->
                Decode.succeed (AnswerNew chain)
                    |> Pipeline.required "pool" (Pool.decoder chain chains)
                    |> Pipeline.required "assetIn" Uint.decoder
                    |> Pipeline.required "debtOut" Uint.decoder
                    |> Pipeline.required "collateralOut" Uint.decoder
                    |> Pipeline.required "result"
                        ([ decoderResultNew |> Decode.map Ok
                         , Error.decoderCreate |> Decode.map Err
                         ]
                            |> Decode.oneOf
                        )
            )


decoderResultNew : Decoder ResultNew
decoderResultNew =
    Decode.succeed ResultNew
        |> Pipeline.required "liquidityOut" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder


toLiquidityGivenNew :
    Result CreateError ResultNew
    -> Remote CreateError LiquidityGivenNew
toLiquidityGivenNew result =
    case result of
        Ok out ->
            Success out

        Err error ->
            Failure error
