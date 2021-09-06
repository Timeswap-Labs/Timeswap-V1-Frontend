module Modals.Pay.Transaction exposing (..)

import Data.Address as Address exposing (Address)
import Data.Allowances as Allowances exposing (Allowances)
import Data.Balances as Balances exposing (Balances)
import Data.Deadline as Deadline exposing (Deadline)
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Maturity as Maturity
import Data.Pair as Pair exposing (Pair)
import Data.Pool exposing (Pool)
import Data.Positions as Positions exposing (Positions)
import Data.Remote exposing (Remote(..))
import Data.Token as Token
import Data.TokenId as TokenId exposing (TokenId)
import Data.Uint as Uint exposing (Uint)
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , el
        , fill
        , height
        , mouseDown
        , mouseOver
        , paddingEach
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Encode as Encode exposing (Value)
import Modals.Pay.DuesIn as DuesIn exposing (DuesIn)
import Sort.Dict as Dict exposing (Dict)
import Sort.Set exposing (Set)
import Time exposing (Posix)
import Utility.Color as Color


type alias Transaction =
    { pool : Pool
    , to : Address
    , dict : Dict TokenId Uint
    , deadline : Int
    }


toTransaction :
    { model
        | time : Posix
        , deadline : Deadline
    }
    ->
        { user
            | address : Address
            , balances : Remote Balances
            , allowances : Remote Allowances
        }
    -> Positions
    ->
        { modal
            | pool : Pool
            , tokenIds : Set TokenId
            , duesIn : Remote { due | assetIn : String }
        }
    -> Maybe Transaction
toTransaction { time, deadline } ({ address } as user) positions ({ pool, tokenIds } as modal) =
    if
        (pool.maturity |> Maturity.isActive time)
            && hasAllowance user modal
    then
        positions
            |> Positions.toDueTransaction pool tokenIds
            |> Maybe.map
                (\dict ->
                    { pool = pool
                    , to = address
                    , dict = dict
                    , deadline = deadline |> Deadline.toInt time
                    }
                )

    else
        Nothing


encode : Transaction -> Value
encode { pool, to, dict, deadline } =
    dict
        |> Dict.toList
        |> List.unzip
        |> (\( ids, maxAssetsIn ) ->
                [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
                , ( "collateral", pool.pair |> Pair.toAsset |> Token.encode )
                , ( "maturity", pool.maturity |> Maturity.encode )
                , ( "collateralTo", to |> Address.encode )
                , ( "ids", ids |> Encode.list TokenId.encode )
                , ( "maxAssetsIn", maxAssetsIn |> Encode.list Uint.encode )
                , ( "deadline", deadline |> Encode.int )
                ]
                    |> Encode.object
           )


hasAllowance :
    { user | balances : Remote Balances, allowances : Remote Allowances }
    -> { modal | pool : { pool | pair : Pair }, duesIn : Remote { due | assetIn : String } }
    -> Bool
hasAllowance { balances, allowances } { pool, duesIn } =
    case ( balances, allowances, duesIn ) of
        ( Success successBalances, Success successAllowances, Success { assetIn } ) ->
            (successBalances
                |> Balances.hasEnough (pool.pair |> Pair.toAsset) assetIn
            )
                && (successAllowances
                        |> Allowances.hasEnough (pool.pair |> Pair.toAsset) assetIn
                   )

        _ ->
            False


view :
    { msgs | approvePay : Value -> msg, pay : Value -> msg }
    ->
        { model
            | device : Device
            , time : Posix
            , deadline : Deadline
            , images : Images
        }
    ->
        { user
            | address : Address
            , balances : Remote Balances
            , allowances : Remote Allowances
        }
    -> Positions
    -> { modal | pool : Pool, tokenIds : Set TokenId, duesIn : DuesIn }
    -> Element msg
view msgs model user positions modal =
    row
        [ width fill
        , height shrink
        , spacing 20
        ]
        [ el
            [ width fill
            , height shrink
            ]
            (approveSection msgs model user positions modal)
        , el
            [ width fill
            , height shrink
            ]
            (paySection msgs model user positions modal)
        ]


approveSection :
    { msgs | approvePay : Value -> msg }
    -> { model | device : Device, time : Posix }
    ->
        { user
            | address : Address
            , balances : Remote Balances
            , allowances : Remote Allowances
        }
    -> Positions
    -> { modal | pool : Pool, tokenIds : Set TokenId, duesIn : DuesIn }
    -> Element msg
approveSection msgs model user positions modal =
    if
        DuesIn.hasTransaction model user positions modal
            && (hasAllowance user modal |> not)
    then
        approveButton msgs model modal

    else
        disabledApprove model


approveButton :
    { msgs | approvePay : Value -> msg }
    -> { model | device : Device }
    -> { modal | pool : { pool | pair : Pair } }
    -> Element msg
approveButton msgs { device } { pool } =
    Input.button
        ([ width fill
         , paddingEach
            { top = 0
            , right = 16
            , bottom = 0
            , left = 10
            }
         , centerX
         , centerY
         , Background.color Color.primary500
         , Border.rounded 4
         , Font.size 16
         , Font.color Color.light100
         , mouseDown [ Background.color Color.primary400 ]
         , mouseOver [ Background.color Color.primary300 ]
         ]
            ++ (if Device.isPhoneOrTablet device then
                    [ height <| px 35 ]

                else
                    [ height <| px 44 ]
               )
        )
        { onPress =
            pool.pair
                |> Pair.toAsset
                |> Token.encode
                |> msgs.approvePay
                |> Just
        , label =
            el
                [ width shrink
                , height shrink
                , centerX
                , centerY
                , Font.bold
                , Font.size 16
                , Font.color Color.light100
                ]
                (text "Approve")
        }


disabledApprove : { model | device : Device } -> Element msg
disabledApprove { device } =
    el
        ([ width fill
         , paddingEach
            { top = 0
            , right = 16
            , bottom = 0
            , left = 10
            }
         , centerX
         , centerY
         , Background.color Color.primary100
         , Border.rounded 4
         , Font.size 16
         , Font.color Color.light100
         ]
            ++ (if Device.isPhoneOrTablet device then
                    [ height <| px 35 ]

                else
                    [ height <| px 44 ]
               )
        )
        (el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.bold
            , Font.size 16
            , Font.color Color.transparent100
            ]
            (text "Approve")
        )


paySection :
    { msgs | pay : Value -> msg }
    ->
        { model
            | device : Device
            , time : Posix
            , deadline : Deadline
        }
    ->
        { user
            | address : Address
            , balances : Remote Balances
            , allowances : Remote Allowances
        }
    -> Positions
    -> { modal | pool : Pool, tokenIds : Set TokenId, duesIn : DuesIn }
    -> Element msg
paySection msgs model user positions modal =
    toTransaction model user positions modal
        |> Maybe.map (payButton msgs model)
        |> Maybe.withDefault (disabledPay model)


payButton :
    { msgs | pay : Value -> msg }
    -> { model | device : Device }
    -> Transaction
    -> Element msg
payButton msgs { device } transaction =
    Input.button
        ([ width fill
         , paddingEach
            { top = 0
            , right = 16
            , bottom = 0
            , left = 10
            }
         , centerX
         , centerY
         , Background.color Color.primary500
         , Border.rounded 4
         , Font.size 16
         , Font.color Color.light100
         , mouseDown [ Background.color Color.primary400 ]
         , mouseOver [ Background.color Color.primary300 ]
         ]
            ++ (if Device.isPhoneOrTablet device then
                    [ height <| px 35 ]

                else
                    [ height <| px 44 ]
               )
        )
        { onPress =
            transaction
                |> encode
                |> msgs.pay
                |> Just
        , label =
            el
                [ width shrink
                , height shrink
                , centerX
                , centerY
                , Font.bold
                , Font.size 16
                , Font.color Color.light100
                ]
                (text "Unlock collateral")
        }


disabledPay : { model | device : Device } -> Element msg
disabledPay { device } =
    el
        ([ width fill
         , paddingEach
            { top = 0
            , right = 16
            , bottom = 0
            , left = 10
            }
         , centerX
         , centerY
         , Background.color Color.primary100
         , Border.rounded 4
         , Font.size 16
         , Font.color Color.light100
         ]
            ++ (if Device.isPhoneOrTablet device then
                    [ height <| px 35 ]

                else
                    [ height <| px 44 ]
               )
        )
        (el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.bold
            , Font.size 16
            , Font.color Color.transparent100
            ]
            (text "Unlock collateral")
        )
