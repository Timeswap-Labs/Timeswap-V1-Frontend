module Page.Transaction.Lend.Answer exposing
    ( Answer(..)
    , ClaimsGivenBond
    , ClaimsGivenInsurance
    , ClaimsGivenPercent
    , decoder
    , initGivenBond
    , initGivenInsurance
    , initGivenPercent
    , toClaimsGivenBond
    , toClaimsGivenInsurance
    , toClaimsGivenPercent
    )

import Data.CDP as CDP exposing (CDP)
import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.Percent as Percent exposing (Percent)
import Data.Pool as Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Slippage as Slippage exposing (Slippage)
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Page.Transaction.Lend.Error as Error exposing (Error)
import Page.Transaction.PoolInfo as PoolInfo exposing (PoolInfo)


type Answer
    = GivenPercent AnswerPercent
    | GivenBond AnswerBond
    | GivenInsurance AnswerInsurance


type alias AnswerPercent =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetIn : Uint
    , percent : Percent
    , slippage : Slippage
    , result : Result Error ResultPercent
    }


type alias AnswerBond =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetIn : Uint
    , bondOut : Uint
    , slippage : Slippage
    , result : Result Error ResultBond
    }


type alias AnswerInsurance =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetIn : Uint
    , insuranceOut : Uint
    , slippage : Slippage
    , result : Result Error ResultInsurance
    }


type alias ResultPercent =
    { bond : Uint
    , insurance : Uint
    , minBond : Uint
    , minInsurance : Uint
    , apr : Float
    , cdp : CDP
    }


type alias ResultBond =
    { percent : Percent
    , insurance : Uint
    , minInsurance : Uint
    , apr : Float
    , cdp : CDP
    }


type alias ResultInsurance =
    { percent : Percent
    , bond : Uint
    , minBond : Uint
    , apr : Float
    , cdp : CDP
    }


type alias ClaimsGivenPercent =
    ResultPercent


type alias ClaimsGivenBond =
    { insurance : Uint
    , minInsurance : Uint
    , apr : Float
    , cdp : CDP
    }


type alias ClaimsGivenInsurance =
    { bond : Uint
    , minBond : Uint
    , apr : Float
    , cdp : CDP
    }


initGivenPercent : ClaimsGivenPercent
initGivenPercent =
    { bond = Uint.zero
    , insurance = Uint.zero
    , minBond = Uint.zero
    , minInsurance = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


initGivenBond : ClaimsGivenBond
initGivenBond =
    { insurance = Uint.zero
    , minInsurance = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


initGivenInsurance : ClaimsGivenInsurance
initGivenInsurance =
    { bond = Uint.zero
    , minBond = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


decoder : { model | chains : Chains } -> Decoder Answer
decoder model =
    [ decoderAnswerPercent model
        |> Decode.map GivenPercent
    , decoderAnswerBond model
        |> Decode.map GivenBond
    , decoderAnswerInsurance model
        |> Decode.map GivenInsurance
    ]
        |> Decode.oneOf


decoderAnswerPercent :
    { model | chains : Chains }
    -> Decoder AnswerPercent
decoderAnswerPercent { chains } =
    Chains.decoderChain chains
        |> Decode.field "chainId"
        |> Decode.andThen
            (\chain ->
                Decode.succeed (AnswerPercent chain)
                    |> Pipeline.required "pool" (Pool.decoder chain chains)
                    |> Pipeline.required "poolInfo" PoolInfo.decoder
                    |> Pipeline.required "assetIn" Uint.decoder
                    |> Pipeline.required "percent" Percent.decoder
                    |> Pipeline.required "slippage" Slippage.decoder
                    |> Pipeline.required "result"
                        ([ decoderResultPercent |> Decode.map Ok
                         , Error.decoder |> Decode.map Err
                         ]
                            |> Decode.oneOf
                        )
            )


decoderAnswerBond :
    { model | chains : Chains }
    -> Decoder AnswerBond
decoderAnswerBond { chains } =
    Chains.decoderChain chains
        |> Decode.field "chainId"
        |> Decode.andThen
            (\chain ->
                Decode.succeed (AnswerBond chain)
                    |> Pipeline.required "pool" (Pool.decoder chain chains)
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
            )


decoderAnswerInsurance :
    { model | chains : Chains }
    -> Decoder AnswerInsurance
decoderAnswerInsurance { chains } =
    Chains.decoderChain chains
        |> Decode.field "chainId"
        |> Decode.andThen
            (\chain ->
                Decode.succeed (AnswerInsurance chain)
                    |> Pipeline.required "pool" (Pool.decoder chain chains)
                    |> Pipeline.required "poolInfo" PoolInfo.decoder
                    |> Pipeline.required "assetIn" Uint.decoder
                    |> Pipeline.required "bondOut" Uint.decoder
                    |> Pipeline.required "slippage" Slippage.decoder
                    |> Pipeline.required "result"
                        ([ decoderResultInsurance |> Decode.map Ok
                         , Error.decoder |> Decode.map Err
                         ]
                            |> Decode.oneOf
                        )
            )


decoderResultPercent : Decoder ResultPercent
decoderResultPercent =
    Decode.succeed ResultPercent
        |> Pipeline.required "bond" Uint.decoder
        |> Pipeline.required "insurance" Uint.decoder
        |> Pipeline.required "minBond" Uint.decoder
        |> Pipeline.required "minInsurance" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder


decoderResultBond : Decoder ResultBond
decoderResultBond =
    Decode.succeed ResultBond
        |> Pipeline.required "percent" Percent.decoder
        |> Pipeline.required "insurance" Uint.decoder
        |> Pipeline.required "minInsurance" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder


decoderResultInsurance : Decoder ResultInsurance
decoderResultInsurance =
    Decode.succeed ResultInsurance
        |> Pipeline.required "percent" Percent.decoder
        |> Pipeline.required "bond" Uint.decoder
        |> Pipeline.required "minBond" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder


toClaimsGivenPercent :
    Result Error ResultPercent
    -> Remote Error ClaimsGivenPercent
toClaimsGivenPercent result =
    case result of
        Ok claims ->
            Success claims

        Err error ->
            Failure error


toClaimsGivenBond :
    Result Error ResultBond
    -> Remote Error ClaimsGivenBond
toClaimsGivenBond result =
    case result of
        Ok { insurance, minInsurance, apr, cdp } ->
            { insurance = insurance
            , minInsurance = minInsurance
            , apr = apr
            , cdp = cdp
            }
                |> Success

        Err error ->
            Failure error


toClaimsGivenInsurance :
    Result Error ResultInsurance
    -> Remote Error ClaimsGivenInsurance
toClaimsGivenInsurance result =
    case result of
        Ok { bond, minBond, apr, cdp } ->
            { bond = bond
            , minBond = minBond
            , apr = apr
            , cdp = cdp
            }
                |> Success

        Err error ->
            Failure error
