module Modals.Lend.Query exposing
    ( Query(..)
    , decoder
    , givenBond
    , givenInsurance
    , givenPercent
    , updateBondQuery
    , updateDefaultQuery
    , updateInsuranceQuery
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
import Modals.Lend.ClaimsOut as ClaimsOut exposing (ClaimsOut)
import Utility.Input as Input


type Query
    = GivenPercent QueryPercent
    | GivenBond QueryBond
    | GivenInsurance QueryInsurance


type alias QueryPercent =
    { pool : Pool
    , assetIn : Uint
    , percent : Percent
    , claims : Maybe ClaimsGivenPercent
    }


type alias ClaimsGivenPercent =
    { bond : Uint
    , insurance : Uint
    , minBond : Uint
    , minInsurance : Uint
    }


type alias QueryBond =
    { pool : Pool
    , assetIn : Uint
    , bond : Uint
    , claims : Maybe ClaimsGivenBond
    }


type alias ClaimsGivenBond =
    { percent : Percent
    , insurance : Uint
    , minInsurance : Uint
    }


type alias QueryInsurance =
    { pool : Pool
    , assetIn : Uint
    , insurance : Uint
    , claims : Maybe ClaimsGivenInsurance
    }


type alias ClaimsGivenInsurance =
    { percent : Percent
    , bond : Uint
    , minBond : Uint
    }


decoder : Pools -> Tokens -> Decoder Query
decoder pools tokens =
    Decode.oneOf
        [ decoderGivenPercent pools tokens
        , decoderGivenBond pools tokens
        , decoderGivenInsurance pools tokens
        ]


decoderGivenPercent : Pools -> Tokens -> Decoder Query
decoderGivenPercent pools tokens =
    Decode.succeed QueryPercent
        |> Pipeline.custom (Pools.decoderPool pools tokens)
        |> Pipeline.required "assetIn" Uint.decoder
        |> Pipeline.required "percent" Percent.decoder
        |> Pipeline.required "claims"
            (Decode.succeed ClaimsGivenPercent
                |> Pipeline.required "bondOut" Uint.decoder
                |> Pipeline.required "insuranceOut" Uint.decoder
                |> Pipeline.required "minBond" Uint.decoder
                |> Pipeline.required "minInsurance" Uint.decoder
                |> Decode.nullable
            )
        |> Decode.map GivenPercent


decoderGivenBond : Pools -> Tokens -> Decoder Query
decoderGivenBond pools tokens =
    Decode.succeed QueryBond
        |> Pipeline.custom (Pools.decoderPool pools tokens)
        |> Pipeline.required "assetIn" Uint.decoder
        |> Pipeline.required "bondOut" Uint.decoder
        |> Pipeline.required "claims"
            (Decode.succeed ClaimsGivenBond
                |> Pipeline.required "percent" Percent.decoder
                |> Pipeline.required "insuranceOut" Uint.decoder
                |> Pipeline.required "minInsurance" Uint.decoder
                |> Decode.nullable
            )
        |> Decode.map GivenBond


decoderGivenInsurance : Pools -> Tokens -> Decoder Query
decoderGivenInsurance pools tokens =
    Decode.succeed QueryInsurance
        |> Pipeline.custom (Pools.decoderPool pools tokens)
        |> Pipeline.required "assetIn" Uint.decoder
        |> Pipeline.required "insuranceOut" Uint.decoder
        |> Pipeline.required "claims"
            (Decode.succeed ClaimsGivenInsurance
                |> Pipeline.required "percent" Percent.decoder
                |> Pipeline.required "bondOut" Uint.decoder
                |> Pipeline.required "minBond" Uint.decoder
                |> Decode.nullable
            )
        |> Decode.map GivenInsurance


givenPercent : Pool -> String -> Percent -> Slippage -> Maybe Value
givenPercent pool assetIn percent slippage =
    if assetIn |> Input.isZero then
        Nothing

    else
        assetIn
            |> Uint.fromString
            |> Maybe.map
                (\uintAssetIn ->
                    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
                    , ( "collateral", pool.pair |> Pair.toAsset |> Token.encode )
                    , ( "maturity", pool.maturity |> Maturity.encode )
                    , ( "assetIn", uintAssetIn |> Uint.encode )
                    , ( "percent", percent |> Percent.encode )
                    , ( "slippage", slippage |> Slippage.encode )
                    ]
                        |> Encode.object
                )


givenBond : Pool -> String -> String -> Slippage -> Maybe Value
givenBond pool assetIn bond slippage =
    if (assetIn |> Input.isZero) || (bond |> Input.isZero) then
        Nothing

    else
        Maybe.map2
            (\uintAssetIn uintBondOut ->
                [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
                , ( "collateral", pool.pair |> Pair.toAsset |> Token.encode )
                , ( "maturity", pool.maturity |> Maturity.encode )
                , ( "assetIn", uintAssetIn |> Uint.encode )
                , ( "bondOut", uintBondOut |> Uint.encode )
                , ( "slippage", slippage |> Slippage.encode )
                ]
                    |> Encode.object
            )
            (assetIn |> Uint.fromString)
            (bond |> Uint.fromString)


givenInsurance : Pool -> String -> String -> Slippage -> Maybe Value
givenInsurance pool assetIn insurance slippage =
    if (assetIn |> Input.isZero) || (insurance |> Input.isZero) then
        Nothing

    else
        Maybe.map2
            (\uintAssetIn uintInsuranceOut ->
                [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
                , ( "collateral", pool.pair |> Pair.toAsset |> Token.encode )
                , ( "maturity", pool.maturity |> Maturity.encode )
                , ( "assetIn", uintAssetIn |> Uint.encode )
                , ( "insuranceOut", uintInsuranceOut |> Uint.encode )
                , ( "slippage", slippage |> Slippage.encode )
                ]
                    |> Encode.object
            )
            (assetIn |> Uint.fromString)
            (insurance |> Uint.fromString)


updateDefaultQuery :
    { modal | pool : { pool | pair : Pair } }
    -> Maybe ClaimsGivenPercent
    -> ClaimsOut
    -> ClaimsOut
updateDefaultQuery { pool } maybeClaims claimsOut =
    case claimsOut of
        ClaimsOut.Default _ ->
            (maybeClaims
                |> Maybe.map
                    (\{ bond, insurance, minBond, minInsurance } ->
                        { bond = bond |> Uint.toAmount (pool.pair |> Pair.toAsset)
                        , insurance = insurance |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                        , minBond = minBond |> Uint.toAmount (pool.pair |> Pair.toAsset)
                        , minInsurance = minInsurance |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                        }
                            |> Success
                    )
                |> Maybe.withDefault Failure
            )
                |> ClaimsOut.Default

        _ ->
            claimsOut


updateSliderQuery :
    { modal | pool : { pool | pair : Pair } }
    -> Maybe ClaimsGivenPercent
    -> ClaimsOut
    -> ClaimsOut
updateSliderQuery { pool } maybeClaims claimsOut =
    case claimsOut of
        ClaimsOut.Slider sliderInput ->
            { sliderInput
                | claims =
                    maybeClaims
                        |> Maybe.map
                            (\{ bond, insurance, minBond, minInsurance } ->
                                { bond = bond |> Uint.toAmount (pool.pair |> Pair.toAsset)
                                , insurance = insurance |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                                , minBond = minBond |> Uint.toAmount (pool.pair |> Pair.toAsset)
                                , minInsurance = minInsurance |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                                }
                                    |> Success
                            )
                        |> Maybe.withDefault Failure
            }
                |> ClaimsOut.Slider

        _ ->
            claimsOut


updateBondQuery :
    { modal | pool : { pool | pair : Pair } }
    -> Maybe ClaimsGivenBond
    -> ClaimsOut
    -> ClaimsOut
updateBondQuery { pool } maybeClaims claimsOut =
    case claimsOut of
        ClaimsOut.Bond bondInput ->
            { bondInput
                | percent =
                    maybeClaims
                        |> Maybe.map .percent
                        |> Maybe.withDefault bondInput.percent
                , claims =
                    maybeClaims
                        |> Maybe.map
                            (\{ insurance, minInsurance } ->
                                { insurance = insurance |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                                , minInsurance = minInsurance |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                                }
                                    |> Success
                            )
                        |> Maybe.withDefault Failure
            }
                |> ClaimsOut.Bond

        _ ->
            claimsOut


updateInsuranceQuery :
    { modal | pool : { pool | pair : Pair } }
    -> Maybe ClaimsGivenInsurance
    -> ClaimsOut
    -> ClaimsOut
updateInsuranceQuery { pool } maybeClaims claimsOut =
    case claimsOut of
        ClaimsOut.Insurance insuranceInput ->
            { insuranceInput
                | percent =
                    maybeClaims
                        |> Maybe.map .percent
                        |> Maybe.withDefault insuranceInput.percent
                , claims =
                    maybeClaims
                        |> Maybe.map
                            (\{ bond, minBond } ->
                                { bond = bond |> Uint.toAmount (pool.pair |> Pair.toAsset)
                                , minBond = minBond |> Uint.toAmount (pool.pair |> Pair.toAsset)
                                }
                                    |> Success
                            )
                        |> Maybe.withDefault Failure
            }
                |> ClaimsOut.Insurance

        _ ->
            claimsOut