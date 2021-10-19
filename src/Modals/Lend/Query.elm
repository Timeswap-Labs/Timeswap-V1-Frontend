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
import Modals.Lend.Error as Error exposing (Error)
import Utility.Input as Input


type Query
    = GivenPercent QueryPercent
    | GivenBond QueryBond
    | GivenInsurance QueryInsurance


type alias QueryPercent =
    { pool : Pool
    , assetIn : Uint
    , percent : Percent
    , claims : Result Error ClaimsGivenPercent
    }


type alias ClaimsGivenPercent =
    { bond : Uint
    , insurance : Uint
    , minBond : Uint
    , minInsurance : Uint
    , apr : Float
    , cf : Uint
    }


type alias QueryBond =
    { pool : Pool
    , assetIn : Uint
    , bond : Uint
    , claims : Result Error ClaimsGivenBond
    }


type alias ClaimsGivenBond =
    { percent : Percent
    , insurance : Uint
    , minInsurance : Uint
    , apr : Float
    , cf : Uint
    }


type alias QueryInsurance =
    { pool : Pool
    , assetIn : Uint
    , insurance : Uint
    , claims : Result Error ClaimsGivenInsurance
    }


type alias ClaimsGivenInsurance =
    { percent : Percent
    , bond : Uint
    , minBond : Uint
    , apr : Float
    , cf : Uint
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
        |> Pipeline.required "result"
            (Decode.oneOf
                [ decoderClaimsGivenPercent |> Decode.map Ok
                , Error.decoder |> Decode.map Err
                ]
            )
        |> Decode.map GivenPercent


decoderClaimsGivenPercent : Decoder ClaimsGivenPercent
decoderClaimsGivenPercent =
    Decode.succeed ClaimsGivenPercent
        |> Pipeline.required "bondOut" Uint.decoder
        |> Pipeline.required "insuranceOut" Uint.decoder
        |> Pipeline.required "minBond" Uint.decoder
        |> Pipeline.required "minInsurance" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cf" Uint.decoder


decoderGivenBond : Pools -> Tokens -> Decoder Query
decoderGivenBond pools tokens =
    Decode.succeed QueryBond
        |> Pipeline.custom (Pools.decoderPool pools tokens)
        |> Pipeline.required "assetIn" Uint.decoder
        |> Pipeline.required "bondOut" Uint.decoder
        |> Pipeline.required "result"
            (Decode.oneOf
                [ decoderClaimsGivenBond |> Decode.map Ok
                , Error.decoder |> Decode.map Err
                ]
            )
        |> Decode.map GivenBond


decoderClaimsGivenBond : Decoder ClaimsGivenBond
decoderClaimsGivenBond =
    Decode.succeed ClaimsGivenBond
        |> Pipeline.required "percent" Percent.decoder
        |> Pipeline.required "insuranceOut" Uint.decoder
        |> Pipeline.required "minInsurance" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cf" Uint.decoder


decoderGivenInsurance : Pools -> Tokens -> Decoder Query
decoderGivenInsurance pools tokens =
    Decode.succeed QueryInsurance
        |> Pipeline.custom (Pools.decoderPool pools tokens)
        |> Pipeline.required "assetIn" Uint.decoder
        |> Pipeline.required "insuranceOut" Uint.decoder
        |> Pipeline.required "result"
            (Decode.oneOf
                [ decoderClaimsGivenInsurance |> Decode.map Ok
                , Error.decoder |> Decode.map Err
                ]
            )
        |> Decode.map GivenInsurance


decoderClaimsGivenInsurance : Decoder ClaimsGivenInsurance
decoderClaimsGivenInsurance =
    Decode.succeed ClaimsGivenInsurance
        |> Pipeline.required "percent" Percent.decoder
        |> Pipeline.required "bondOut" Uint.decoder
        |> Pipeline.required "minBond" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cf" Uint.decoder


givenPercent : Pool -> String -> Percent -> Slippage -> Maybe Value
givenPercent pool assetIn percent slippage =
    if assetIn |> Input.isZero then
        Nothing

    else
        assetIn
            |> Uint.fromAmount (pool.pair |> Pair.toAsset)
            |> Maybe.map
                (\uintAssetIn ->
                    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
                    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
                    , ( "maturity", pool.maturity |> Maturity.encode )
                    , ( "assetIn", uintAssetIn |> Uint.encode )
                    , ( "percent", percent |> Percent.encode )
                    , ( "slippage", slippage |> Slippage.encodeGivenPercent )
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
                , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
                , ( "maturity", pool.maturity |> Maturity.encode )
                , ( "assetIn", uintAssetIn |> Uint.encode )
                , ( "bondOut", uintBondOut |> Uint.encode )
                , ( "slippage", slippage |> Slippage.encode )
                ]
                    |> Encode.object
            )
            (assetIn |> Uint.fromAmount (pool.pair |> Pair.toAsset))
            (bond |> Uint.fromAmount (pool.pair |> Pair.toAsset))


givenInsurance : Pool -> String -> String -> Slippage -> Maybe Value
givenInsurance pool assetIn insurance slippage =
    if (assetIn |> Input.isZero) || (insurance |> Input.isZero) then
        Nothing

    else
        Maybe.map2
            (\uintAssetIn uintInsuranceOut ->
                [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
                , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
                , ( "maturity", pool.maturity |> Maturity.encode )
                , ( "assetIn", uintAssetIn |> Uint.encode )
                , ( "insuranceOut", uintInsuranceOut |> Uint.encode )
                , ( "slippage", slippage |> Slippage.encode )
                ]
                    |> Encode.object
            )
            (assetIn |> Uint.fromAmount (pool.pair |> Pair.toAsset))
            (insurance |> Uint.fromAmount (pool.pair |> Pair.toCollateral))


toAPR : Float -> String
toAPR float =
    float
        |> (*) 10000
        |> truncate
        |> String.fromInt
        |> String.padLeft 3 '0'
        |> (\string ->
                [ string |> String.dropRight 2
                , string |> String.right 2
                ]
                    |> String.join "."
           )


updateDefaultQuery :
    { modal | pool : { pool | pair : Pair } }
    -> Result Error ClaimsGivenPercent
    -> ClaimsOut
    -> ClaimsOut
updateDefaultQuery { pool } resultClaims claimsOut =
    case claimsOut of
        ClaimsOut.Default _ ->
            (case resultClaims of
                Ok { bond, insurance, minBond, minInsurance, apr, cf } ->
                    { bond = bond |> Uint.toAmount (pool.pair |> Pair.toAsset)
                    , insurance = insurance |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                    , minBond = minBond |> Uint.toAmount (pool.pair |> Pair.toAsset)
                    , minInsurance = minInsurance |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                    , apr = apr |> toAPR
                    , cf = cf |> Uint.toAmount (pool.pair |> Pair.toAsset)
                    }
                        |> Success

                Err error ->
                    Failure error
            )
                |> ClaimsOut.Default

        _ ->
            claimsOut


updateSliderQuery :
    { modal | pool : { pool | pair : Pair } }
    -> Result Error ClaimsGivenPercent
    -> ClaimsOut
    -> ClaimsOut
updateSliderQuery { pool } resultClaims claimsOut =
    case claimsOut of
        ClaimsOut.Slider sliderInput ->
            { sliderInput
                | claims =
                    case resultClaims of
                        Ok { bond, insurance, minBond, minInsurance, apr, cf } ->
                            { bond = bond |> Uint.toAmount (pool.pair |> Pair.toAsset)
                            , insurance = insurance |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                            , minBond = minBond |> Uint.toAmount (pool.pair |> Pair.toAsset)
                            , minInsurance = minInsurance |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                            , apr = apr |> toAPR
                            , cf = cf |> Uint.toAmount (pool.pair |> Pair.toAsset)
                            }
                                |> Success

                        Err error ->
                            Failure error
            }
                |> ClaimsOut.Slider

        _ ->
            claimsOut


updateBondQuery :
    { modal | pool : { pool | pair : Pair } }
    -> Result Error ClaimsGivenBond
    -> ClaimsOut
    -> ClaimsOut
updateBondQuery { pool } resultClaims claimsOut =
    case claimsOut of
        ClaimsOut.Bond bondInput ->
            { bondInput
                | percent =
                    resultClaims
                        |> Result.map .percent
                        |> Result.withDefault bondInput.percent
                , claims =
                    case resultClaims of
                        Ok { insurance, minInsurance, apr, cf } ->
                            { insurance = insurance |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                            , minInsurance = minInsurance |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                            , apr = apr |> toAPR
                            , cf = cf |> Uint.toAmount (pool.pair |> Pair.toAsset)
                            }
                                |> Success

                        Err error ->
                            Failure error
            }
                |> ClaimsOut.Bond

        _ ->
            claimsOut


updateInsuranceQuery :
    { modal | pool : { pool | pair : Pair } }
    -> Result Error ClaimsGivenInsurance
    -> ClaimsOut
    -> ClaimsOut
updateInsuranceQuery { pool } resultClaims claimsOut =
    case claimsOut of
        ClaimsOut.Insurance insuranceInput ->
            { insuranceInput
                | percent =
                    resultClaims
                        |> Result.map .percent
                        |> Result.withDefault insuranceInput.percent
                , claims =
                    case resultClaims of
                        Ok { bond, minBond, apr, cf } ->
                            { bond = bond |> Uint.toAmount (pool.pair |> Pair.toAsset)
                            , minBond = minBond |> Uint.toAmount (pool.pair |> Pair.toAsset)
                            , apr = apr |> toAPR
                            , cf = cf |> Uint.toAmount (pool.pair |> Pair.toAsset)
                            }
                                |> Success

                        Err error ->
                            Failure error
            }
                |> ClaimsOut.Insurance

        _ ->
            claimsOut
