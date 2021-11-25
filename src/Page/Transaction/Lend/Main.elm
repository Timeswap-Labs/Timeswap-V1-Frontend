module Page.Transaction.Lend.Main exposing
    ( Effect(..)
    , Msg
    , Section
    , init
    , initGivenPool
    , toParameter
    , toPoolInfo
    , update
    )

import Data.CDP as CDP exposing (CDP)
import Data.Maturity as Maturity
import Data.Pair as Pair exposing (Pair)
import Data.Parameter as Parameter exposing (Parameter)
import Data.Percent exposing (Percent)
import Data.Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Token exposing (Token)
import Data.TokenParam exposing (TokenParam)
import Data.Uint as Uint exposing (Uint)
import Page.Transaction.Error exposing (Error)
import Page.Transaction.Lend.Error as Lend
import Page.Transaction.Lend.Tooltip exposing (Tooltip)
import Page.Transaction.PoolInfo exposing (PoolInfo)
import Page.Transaction.State as State exposing (State)
import Time exposing (Posix)
import Utility.Input as Input


type Section
    = None
    | Asset Token
    | Collateral Token
    | Pair Pair
    | Pool
        Pool
        (Remote
            (Error Transaction)
            (State Transaction ())
        )


type alias Transaction =
    { assetIn : String
    , claimsOut : ClaimsOut
    , tooltip : Maybe Tooltip
    }


type ClaimsOut
    = Default (Remote Lend.Error ClaimsGivenPercent)
    | Slider SliderInput
    | Bond BondInput
    | Insurance InsuranceInput


type alias SliderInput =
    { percent : Percent
    , claims : Remote Lend.Error ClaimsGivenPercent
    }


type alias BondInput =
    { percent : Percent
    , bond : String
    , claims : Remote Lend.Error ClaimsGivenBond
    }


type alias InsuranceInput =
    { percent : Percent
    , claims : Remote Lend.Error ClaimsGivenInsurance
    , insurance : String
    }


type alias ClaimsGivenPercent =
    { bond : Uint
    , insurance : Uint
    , minBond : Uint
    , minInsurance : Uint
    , apr : Float
    , cdp : CDP
    }


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


type Msg
    = SelectToken TokenParam
    | SelectMaturity
    | InputAssetIn String


type Effect
    = OpenTokenList TokenParam
    | OpenMaturityList Pair


init :
    { model | time : Posix }
    -> Maybe Parameter
    -> ( Section, Cmd Msg )
init { time } parameter =
    case parameter of
        Nothing ->
            ( None
            , Cmd.none
            )

        Just (Parameter.Asset asset) ->
            ( Asset asset
            , Cmd.none
            )

        Just (Parameter.Collateral collateral) ->
            ( Collateral collateral
            , Cmd.none
            )

        Just (Parameter.Pair pair) ->
            ( Pair pair
            , Cmd.none
            )

        Just (Parameter.Pool pool) ->
            if pool.maturity |> Maturity.isActive time then
                ( Loading |> Pool pool
                , Debug.todo "http call"
                )

            else
                ( State.Matured
                    |> Success
                    |> Pool pool
                , Cmd.none
                )


initGivenPool :
    { model | time : Posix }
    -> Pool
    -> PoolInfo
    -> Section
initGivenPool { time } pool poolInfo =
    (if pool.maturity |> Maturity.isActive time then
        initTransaction
            |> State.Active poolInfo

     else
        State.Matured
    )
        |> Success
        |> Pool pool


initTransaction : Transaction
initTransaction =
    { assetIn = ""
    , claimsOut =
        { bond = Uint.zero
        , insurance = Uint.zero
        , minBond = Uint.zero
        , minInsurance = Uint.zero
        , apr = 0
        , cdp = CDP.init
        }
            |> Success
            |> Default
    , tooltip = Nothing
    }


update : Msg -> Section -> ( Section, Cmd Msg, Maybe Effect )
update msg section =
    case ( msg, section ) of
        ( SelectToken tokenParam, _ ) ->
            ( section
            , Cmd.none
            , tokenParam
                |> OpenTokenList
                |> Just
            )

        ( SelectMaturity, Pair pair ) ->
            ( section
            , Cmd.none
            , pair
                |> OpenMaturityList
                |> Just
            )

        ( SelectMaturity, Pool pool _ ) ->
            ( section
            , Cmd.none
            , pool.pair
                |> OpenMaturityList
                |> Just
            )

        ( InputAssetIn string, Pool pool (Success (State.Active poolInfo transaction)) ) ->
            if string |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                ( { transaction
                    | assetIn = string
                    , claimsOut =
                        if string |> Input.isZero then
                            transaction.claimsOut |> updateGivenAssetInZero

                        else
                            transaction.claimsOut |> updateGivenAssetIn
                  }
                    |> State.Active poolInfo
                    |> Success
                    |> Pool pool
                , Debug.todo "wrapper"
                , Nothing
                )

            else
                ( transaction
                    |> State.Active poolInfo
                    |> Success
                    |> Pool pool
                , Cmd.none
                , Nothing
                )

        _ ->
            ( section
            , Cmd.none
            , Nothing
            )


updateGivenAssetInZero : ClaimsOut -> ClaimsOut
updateGivenAssetInZero claimsOut =
    case claimsOut of
        Default _ ->
            { bond = Uint.zero
            , insurance = Uint.zero
            , minBond = Uint.zero
            , minInsurance = Uint.zero
            , apr = 0
            , cdp = CDP.init
            }
                |> Success
                |> Default

        Slider slider ->
            { slider
                | claims =
                    { bond = Uint.zero
                    , insurance = Uint.zero
                    , minBond = Uint.zero
                    , minInsurance = Uint.zero
                    , apr = 0
                    , cdp = CDP.init
                    }
                        |> Success
            }
                |> Slider

        Bond bond ->
            { bond
                | claims =
                    { insurance = Uint.zero
                    , minInsurance = Uint.zero
                    , apr = 0
                    , cdp = CDP.init
                    }
                        |> Success
            }
                |> Bond

        Insurance insurance ->
            { insurance
                | claims =
                    { bond = Uint.zero
                    , minBond = Uint.zero
                    , apr = 0
                    , cdp = CDP.init
                    }
                        |> Success
            }
                |> Insurance


updateGivenAssetIn : ClaimsOut -> ClaimsOut
updateGivenAssetIn claimsOut =
    case claimsOut of
        Default _ ->
            Default Loading

        Slider slider ->
            { slider | claims = Loading }
                |> Slider

        Bond bond ->
            { bond
                | claims =
                    if bond.bond |> Input.isZero then
                        { insurance = Uint.zero
                        , minInsurance = Uint.zero
                        , apr = 0
                        , cdp = CDP.init
                        }
                            |> Success

                    else
                        Loading
            }
                |> Bond

        Insurance insurance ->
            { insurance
                | claims =
                    if insurance.insurance |> Input.isZero then
                        { bond = Uint.zero
                        , minBond = Uint.zero
                        , apr = 0
                        , cdp = CDP.init
                        }
                            |> Success

                    else
                        Loading
            }
                |> Insurance


toParameter : Section -> Maybe Parameter
toParameter section =
    case section of
        None ->
            Nothing

        Asset asset ->
            Parameter.Asset asset
                |> Just

        Collateral collateral ->
            Parameter.Collateral collateral
                |> Just

        Pair pair ->
            Parameter.Pair pair
                |> Just

        Pool pool _ ->
            Parameter.Pool pool
                |> Just


toPoolInfo : Section -> Maybe PoolInfo
toPoolInfo section =
    case section of
        Pool _ (Success (State.Active poolInfo _)) ->
            poolInfo |> Just

        _ ->
            Nothing
