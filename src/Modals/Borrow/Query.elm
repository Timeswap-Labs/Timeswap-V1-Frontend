module Modals.Borrow.Query exposing
    ( Query(..)
    , decoder
    , givenCollateral
    , givenDebt
    , givenPercent
    , updateCollateralQuery
    , updateDebtQuery
    , updateDefaultQuery
    , updateMaxQuery
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
import Modals.Borrow.Error as Error exposing (Error)
import Utility.Input as Input


type Query
    = GivenPercent QueryPercent
    | GivenDebt QueryDebt
    | GivenCollateral QueryCollateral



-- | GivenMax QueryMax


type alias QueryPercent =
    { pool : Pool
    , assetOut : Uint
    , percent : Percent
    , dues : Result Error DuesGivenPercent
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
    , dues : Result Error DuesGivenDebt
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
    , dues : Result Error DuesGivenCollateral
    }


type alias DuesGivenCollateral =
    { percent : Percent
    , debt : Uint
    , maxDebt : Uint
    , apr : Float
    , cf : Uint
    }


type alias DuesGivenMax =
    { percent : Percent
    , debt : Uint
    , collateral : Uint
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
        |> Pipeline.required "result"
            (Decode.oneOf
                [ decoderDuesGivenPercent |> Decode.map Ok
                , Error.decoder |> Decode.map Err
                ]
            )
        |> Decode.map GivenPercent


decoderDuesGivenPercent : Decoder DuesGivenPercent
decoderDuesGivenPercent =
    Decode.succeed DuesGivenPercent
        |> Pipeline.required "debtIn" Uint.decoder
        |> Pipeline.required "collateralIn" Uint.decoder
        |> Pipeline.required "maxDebt" Uint.decoder
        |> Pipeline.required "maxCollateral" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cf" Uint.decoder


decoderGivenDebt : Pools -> Tokens -> Decoder Query
decoderGivenDebt pools tokens =
    Decode.succeed QueryDebt
        |> Pipeline.custom (Pools.decoderPool pools tokens)
        |> Pipeline.required "assetOut" Uint.decoder
        |> Pipeline.required "debtIn" Uint.decoder
        |> Pipeline.required "result"
            (Decode.oneOf
                [ decoderDuesGivenDebt |> Decode.map Ok
                , Error.decoder |> Decode.map Err
                ]
            )
        |> Decode.map GivenDebt


decoderDuesGivenDebt : Decoder DuesGivenDebt
decoderDuesGivenDebt =
    Decode.succeed DuesGivenDebt
        |> Pipeline.required "percent" Percent.decoder
        |> Pipeline.required "collateralIn" Uint.decoder
        |> Pipeline.required "maxCollateral" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cf" Uint.decoder


decoderGivenCollateral : Pools -> Tokens -> Decoder Query
decoderGivenCollateral pools tokens =
    Decode.succeed QueryCollateral
        |> Pipeline.custom (Pools.decoderPool pools tokens)
        |> Pipeline.required "assetOut" Uint.decoder
        |> Pipeline.required "collateralIn" Uint.decoder
        |> Pipeline.required "result"
            (Decode.oneOf
                [ decoderDuesGivenCollateral |> Decode.map Ok
                , Error.decoder |> Decode.map Err
                ]
            )
        |> Decode.map GivenCollateral


decoderDuesGivenCollateral : Decoder DuesGivenCollateral
decoderDuesGivenCollateral =
    Decode.succeed DuesGivenCollateral
        |> Pipeline.required "percent" Percent.decoder
        |> Pipeline.required "debtIn" Uint.decoder
        |> Pipeline.required "maxDebt" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cf" Uint.decoder


givenPercent : Pool -> String -> Percent -> Slippage -> Maybe Value
givenPercent pool assetOut percent slippage =
    if assetOut |> Input.isZero then
        Nothing

    else
        assetOut
            |> Uint.fromAmount (pool.pair |> Pair.toAsset)
            |> Maybe.map
                (\uintAssetOut ->
                    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
                    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
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
                , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
                , ( "maturity", pool.maturity |> Maturity.encode )
                , ( "assetOut", uintAssetOut |> Uint.encode )
                , ( "debtIn", uintDebtIn |> Uint.encode )
                , ( "slippage", slippage |> Slippage.encode )
                ]
                    |> Encode.object
            )
            (assetOut |> Uint.fromAmount (pool.pair |> Pair.toAsset))
            (debt |> Uint.fromAmount (pool.pair |> Pair.toAsset))


givenCollateral : Pool -> String -> String -> Slippage -> Maybe Value
givenCollateral pool assetOut collateralIn slippage =
    if (assetOut |> Input.isZero) || (collateralIn |> Input.isZero) then
        Nothing

    else
        Maybe.map2
            (\uintAssetOut uintCollateralIn ->
                [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
                , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
                , ( "maturity", pool.maturity |> Maturity.encode )
                , ( "assetOut", uintAssetOut |> Uint.encode )
                , ( "collateralIn", uintCollateralIn |> Uint.encode )
                , ( "slippage", slippage |> Slippage.encode )
                ]
                    |> Encode.object
            )
            (assetOut |> Uint.fromAmount (pool.pair |> Pair.toAsset))
            (collateralIn |> Uint.fromAmount (pool.pair |> Pair.toCollateral))


toAPR : Float -> String
toAPR float =
    float
        |> (*) 10000
        |> String.fromFloat
        |> String.padLeft 3 '0'
        |> String.split "."
        |> List.head
        |> Maybe.map
            (\string ->
                [ string |> String.dropRight 2
                , string |> String.right 2
                ]
                    |> String.join "."
            )
        |> Maybe.withDefault ""


updateDefaultQuery :
    { modal | pool : { pool | pair : Pair } }
    -> Result Error DuesGivenPercent
    -> DuesOut
    -> DuesOut
updateDefaultQuery { pool } resultDues duesOut =
    case duesOut of
        DuesOut.Default _ ->
            (case resultDues of
                Ok { debt, collateral, maxDebt, maxCollateral, apr, cf } ->
                    { debt = debt |> Uint.toAmount (pool.pair |> Pair.toAsset)
                    , collateral = collateral |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                    , maxDebt = maxDebt |> Uint.toAmount (pool.pair |> Pair.toAsset)
                    , maxCollateral = maxCollateral |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                    , apr = apr |> toAPR
                    , cf = cf |> Uint.toAmount (pool.pair |> Pair.toAsset)
                    }
                        |> Success

                Err error ->
                    Failure error
            )
                |> DuesOut.Default

        _ ->
            duesOut


updateSliderQuery :
    { modal | pool : { pool | pair : Pair } }
    -> Result Error DuesGivenPercent
    -> DuesOut
    -> DuesOut
updateSliderQuery { pool } resultDues duesOut =
    case duesOut of
        DuesOut.Slider sliderInput ->
            { sliderInput
                | dues =
                    case resultDues of
                        Ok { debt, collateral, maxDebt, maxCollateral, apr, cf } ->
                            { debt = debt |> Uint.toAmount (pool.pair |> Pair.toAsset)
                            , collateral = collateral |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                            , maxDebt = maxDebt |> Uint.toAmount (pool.pair |> Pair.toAsset)
                            , maxCollateral = maxCollateral |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                            , apr = apr |> toAPR
                            , cf = cf |> Uint.toAmount (pool.pair |> Pair.toAsset)
                            }
                                |> Success

                        Err error ->
                            Failure error
            }
                |> DuesOut.Slider

        _ ->
            duesOut


updateDebtQuery :
    { modal | pool : { pool | pair : Pair } }
    -> Result Error DuesGivenDebt
    -> DuesOut
    -> DuesOut
updateDebtQuery { pool } resultDues duesOut =
    case duesOut of
        DuesOut.Debt debtInput ->
            { debtInput
                | percent =
                    resultDues
                        |> Result.map .percent
                        |> Result.withDefault debtInput.percent
                , dues =
                    case resultDues of
                        Ok { collateral, maxCollateral, apr, cf } ->
                            { collateral = collateral |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                            , maxCollateral = maxCollateral |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                            , apr = apr |> toAPR
                            , cf = cf |> Uint.toAmount (pool.pair |> Pair.toAsset)
                            }
                                |> Success

                        Err error ->
                            Failure error
            }
                |> DuesOut.Debt

        _ ->
            duesOut


updateCollateralQuery :
    { modal | pool : { pool | pair : Pair } }
    -> Result Error DuesGivenCollateral
    -> DuesOut
    -> DuesOut
updateCollateralQuery { pool } resultDues duesOut =
    case duesOut of
        DuesOut.Collateral collateralInput ->
            { collateralInput
                | percent =
                    resultDues
                        |> Result.map .percent
                        |> Result.withDefault collateralInput.percent
                , dues =
                    case resultDues of
                        Ok { debt, maxDebt, apr, cf } ->
                            { debt = debt |> Uint.toAmount (pool.pair |> Pair.toAsset)
                            , maxDebt = maxDebt |> Uint.toAmount (pool.pair |> Pair.toAsset)
                            , apr = apr |> toAPR
                            , cf = cf |> Uint.toAmount (pool.pair |> Pair.toAsset)
                            }
                                |> Success

                        Err error ->
                            Failure error
            }
                |> DuesOut.Collateral

        _ ->
            duesOut


updateMaxQuery :
    { modal | pool : { pool | pair : Pair } }
    -> Result Error DuesGivenMax
    -> DuesOut
    -> DuesOut
updateMaxQuery { pool } resultDues duesOut =
    case duesOut of
        DuesOut.Collateral collateralInput ->
            (case resultDues of
                Ok { percent, debt, collateral, maxDebt, apr, cf } ->
                    { collateralInput
                        | percent = percent
                        , dues =
                            { debt = debt |> Uint.toAmount (pool.pair |> Pair.toAsset)
                            , maxDebt = maxDebt |> Uint.toAmount (pool.pair |> Pair.toAsset)
                            , apr = apr |> toAPR
                            , cf = cf |> Uint.toAmount (pool.pair |> Pair.toAsset)
                            }
                                |> Success
                        , collateral = collateral |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                    }

                Err error ->
                    { collateralInput
                        | percent = collateralInput.percent
                        , dues = Failure error
                        , collateral = collateralInput.collateral
                    }
            )
                |> DuesOut.Collateral

        _ ->
            duesOut
