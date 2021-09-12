module Modals.Borrow.Query exposing
    ( Query(..)
    , decoder
    , givenCollateral
    , givenDebt
    , givenPercent
    , updateCollateralQuery
    , updateDebtQuery
    , updateDefaultQuery
    , updateSliderQuery
    )

import Data.Maturity as Maturity
import Data.Pair as Pair exposing (Pair)
import Data.Percent as Percent exposing (Percent)
import Data.Pool exposing (Pool)
import Data.Pools as Pools exposing (Pools)
import Data.Remote exposing (Remote(..))
import Data.Slippage as Slippage exposing (Slippage)
import Data.Token as Token
import Data.Tokens exposing (Tokens)
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Modals.Borrow.DuesOut as DuesOut exposing (DuesOut)
import Utility.Input as Input


type Query
    = GivenPercent QueryPercent
    | GivenDebt QueryDebt
    | GivenCollateral QueryCollateral


type alias QueryPercent =
    { pool : Pool
    , assetOut : Uint
    , percent : Percent
    , dues : Maybe DuesGivenPercent
    }


type alias DuesGivenPercent =
    { debt : Uint
    , collateral : Uint
    , maxDebt : Uint
    , maxCollateral : Uint
    , apr : Float
    , cf : Uint
    }


type alias QueryDebt =
    { pool : Pool
    , assetOut : Uint
    , debt : Uint
    , dues : Maybe DuesGivenDebt
    }


type alias DuesGivenDebt =
    { percent : Percent
    , collateral : Uint
    , maxCollateral : Uint
    , apr : Float
    , cf : Uint
    }


type alias QueryCollateral =
    { pool : Pool
    , assetOut : Uint
    , collateral : Uint
    , dues : Maybe DuesGivenCollateral
    }


type alias DuesGivenCollateral =
    { percent : Percent
    , debt : Uint
    , maxDebt : Uint
    , apr : Float
    , cf : Uint
    }


decoder : Pools -> Tokens -> Decoder Query
decoder pools tokens =
    Decode.oneOf
        [ decoderGivenPercent pools tokens
        , decoderGivenDebt pools tokens
        , decoderGivenCollateral pools tokens
        ]


decoderGivenPercent : Pools -> Tokens -> Decoder Query
decoderGivenPercent pools tokens =
    Decode.succeed QueryPercent
        |> Pipeline.custom (Pools.decoderPool pools tokens)
        |> Pipeline.required "assetOut" Uint.decoder
        |> Pipeline.required "percent" Percent.decoder
        |> Pipeline.required "dues"
            (Decode.succeed DuesGivenPercent
                |> Pipeline.required "debtIn" Uint.decoder
                |> Pipeline.required "collateralIn" Uint.decoder
                |> Pipeline.required "maxDebt" Uint.decoder
                |> Pipeline.required "maxCollateral" Uint.decoder
                |> Pipeline.required "apr" Decode.float
                |> Pipeline.required "cf" Uint.decoder
                |> Decode.nullable
            )
        |> Decode.map GivenPercent


decoderGivenDebt : Pools -> Tokens -> Decoder Query
decoderGivenDebt pools tokens =
    Decode.succeed QueryDebt
        |> Pipeline.custom (Pools.decoderPool pools tokens)
        |> Pipeline.required "assetOut" Uint.decoder
        |> Pipeline.required "debtIn" Uint.decoder
        |> Pipeline.required "dues"
            (Decode.succeed DuesGivenDebt
                |> Pipeline.required "percent" Percent.decoder
                |> Pipeline.required "collateralIn" Uint.decoder
                |> Pipeline.required "maxCollateral" Uint.decoder
                |> Pipeline.required "apr" Decode.float
                |> Pipeline.required "cf" Uint.decoder
                |> Decode.nullable
            )
        |> Decode.map GivenDebt


decoderGivenCollateral : Pools -> Tokens -> Decoder Query
decoderGivenCollateral pools tokens =
    Decode.succeed QueryCollateral
        |> Pipeline.custom (Pools.decoderPool pools tokens)
        |> Pipeline.required "assetOut" Uint.decoder
        |> Pipeline.required "collateralIn" Uint.decoder
        |> Pipeline.required "claims"
            (Decode.succeed DuesGivenCollateral
                |> Pipeline.required "percent" Percent.decoder
                |> Pipeline.required "debtIn" Uint.decoder
                |> Pipeline.required "maxDebt" Uint.decoder
                |> Pipeline.required "apr" Decode.float
                |> Pipeline.required "cf" Uint.decoder
                |> Decode.nullable
            )
        |> Decode.map GivenCollateral


givenPercent : Pool -> String -> Percent -> Slippage -> Maybe Value
givenPercent pool assetOut percent slippage =
    if assetOut |> Input.isZero then
        Nothing

    else
        assetOut
            |> Uint.fromString
            |> Maybe.map
                (\uintAssetOut ->
                    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
                    , ( "collateral", pool.pair |> Pair.toAsset |> Token.encode )
                    , ( "maturity", pool.maturity |> Maturity.encode )
                    , ( "assetOut", uintAssetOut |> Uint.encode )
                    , ( "percent", percent |> Percent.encode )
                    , ( "slippage", slippage |> Slippage.encodeGivenPercent )
                    ]
                        |> Encode.object
                )


givenDebt : Pool -> String -> String -> Slippage -> Maybe Value
givenDebt pool assetOut debt slippage =
    if (assetOut |> Input.isZero) || (debt |> Input.isZero) then
        Nothing

    else
        Maybe.map2
            (\uintAssetOut uintDebtIn ->
                [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
                , ( "collateral", pool.pair |> Pair.toAsset |> Token.encode )
                , ( "maturity", pool.maturity |> Maturity.encode )
                , ( "assetOut", uintAssetOut |> Uint.encode )
                , ( "debtIn", uintDebtIn |> Uint.encode )
                , ( "slippage", slippage |> Slippage.encode )
                ]
                    |> Encode.object
            )
            (assetOut |> Uint.fromString)
            (debt |> Uint.fromString)


givenCollateral : Pool -> String -> String -> Slippage -> Maybe Value
givenCollateral pool assetOut collateralIn slippage =
    if (assetOut |> Input.isZero) || (collateralIn |> Input.isZero) then
        Nothing

    else
        Maybe.map2
            (\uintAssetOut uintCollateralIn ->
                [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
                , ( "collateral", pool.pair |> Pair.toAsset |> Token.encode )
                , ( "maturity", pool.maturity |> Maturity.encode )
                , ( "assetOut", uintAssetOut |> Uint.encode )
                , ( "collateralIn", uintCollateralIn |> Uint.encode )
                , ( "slippage", slippage |> Slippage.encode )
                ]
                    |> Encode.object
            )
            (assetOut |> Uint.fromString)
            (collateralIn |> Uint.fromString)


toAPR : Float -> String
toAPR float =
    float
        |> (*) 10000
        |> truncate
        |> String.fromInt
        |> String.padRight 3 '0'
        |> (\string ->
                [ string |> String.dropRight 2
                , string |> String.right 2
                ]
                    |> String.join "."
           )


updateDefaultQuery :
    { modal | pool : { pool | pair : Pair } }
    -> Maybe DuesGivenPercent
    -> DuesOut
    -> DuesOut
updateDefaultQuery { pool } maybeDues duesOut =
    case duesOut of
        DuesOut.Default _ ->
            (maybeDues
                |> Maybe.map
                    (\{ debt, collateral, maxDebt, maxCollateral, apr, cf } ->
                        { debt = debt |> Uint.toAmount (pool.pair |> Pair.toAsset)
                        , collateral = collateral |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                        , maxDebt = maxDebt |> Uint.toAmount (pool.pair |> Pair.toAsset)
                        , maxCollateral = maxCollateral |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                        , apr = apr |> toAPR
                        , cf = cf |> Uint.toAmount (pool.pair |> Pair.toAsset)
                        }
                            |> Success
                    )
                |> Maybe.withDefault (Failure ())
            )
                |> DuesOut.Default

        _ ->
            duesOut


updateSliderQuery :
    { modal | pool : { pool | pair : Pair } }
    -> Maybe DuesGivenPercent
    -> DuesOut
    -> DuesOut
updateSliderQuery { pool } maybeDues duesOut =
    case duesOut of
        DuesOut.Slider sliderInput ->
            { sliderInput
                | dues =
                    maybeDues
                        |> Maybe.map
                            (\{ debt, collateral, maxDebt, maxCollateral, apr, cf } ->
                                { debt = debt |> Uint.toAmount (pool.pair |> Pair.toAsset)
                                , collateral = collateral |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                                , maxDebt = maxDebt |> Uint.toAmount (pool.pair |> Pair.toAsset)
                                , maxCollateral = maxCollateral |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                                , apr = apr |> toAPR
                                , cf = cf |> Uint.toAmount (pool.pair |> Pair.toAsset)
                                }
                                    |> Success
                            )
                        |> Maybe.withDefault (Failure ())
            }
                |> DuesOut.Slider

        _ ->
            duesOut


updateDebtQuery :
    { modal | pool : { pool | pair : Pair } }
    -> Maybe DuesGivenDebt
    -> DuesOut
    -> DuesOut
updateDebtQuery { pool } maybeDues duesOut =
    case duesOut of
        DuesOut.Debt debtInput ->
            { debtInput
                | percent =
                    maybeDues
                        |> Maybe.map .percent
                        |> Maybe.withDefault debtInput.percent
                , dues =
                    maybeDues
                        |> Maybe.map
                            (\{ collateral, maxCollateral, apr, cf } ->
                                { collateral = collateral |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                                , maxCollateral = maxCollateral |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                                , apr = apr |> toAPR
                                , cf = cf |> Uint.toAmount (pool.pair |> Pair.toAsset)
                                }
                                    |> Success
                            )
                        |> Maybe.withDefault (Failure ())
            }
                |> DuesOut.Debt

        _ ->
            duesOut


updateCollateralQuery :
    { modal | pool : { pool | pair : Pair } }
    -> Maybe DuesGivenCollateral
    -> DuesOut
    -> DuesOut
updateCollateralQuery { pool } maybeDues duesOut =
    case duesOut of
        DuesOut.Collateral collateralInput ->
            { collateralInput
                | percent =
                    maybeDues
                        |> Maybe.map .percent
                        |> Maybe.withDefault collateralInput.percent
                , dues =
                    maybeDues
                        |> Maybe.map
                            (\{ debt, maxDebt, apr, cf } ->
                                { debt = debt |> Uint.toAmount (pool.pair |> Pair.toAsset)
                                , maxDebt = maxDebt |> Uint.toAmount (pool.pair |> Pair.toAsset)
                                , apr = apr |> toAPR
                                , cf = cf |> Uint.toAmount (pool.pair |> Pair.toAsset)
                                }
                                    |> Success
                            )
                        |> Maybe.withDefault (Failure ())
            }
                |> DuesOut.Collateral

        _ ->
            duesOut
