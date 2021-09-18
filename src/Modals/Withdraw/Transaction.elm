module Modals.Withdraw.Transaction exposing (view)

import Data.Address as Address exposing (Address)
import Data.Device as Device exposing (Device)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Positions as Positions exposing (Positions)
import Data.Token as Token
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
        , shrink
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Encode as Encode exposing (Value)
import Time exposing (Posix)
import Utility.Color as Color


type alias Transaction =
    { pool : Pool
    , to : Address
    , claimsIn :
        { bond : Uint
        , insurance : Uint
        }
    }


toTransaction :
    { model | time : Posix }
    -> { user | address : Address }
    -> Positions
    -> Pool
    -> Maybe Transaction
toTransaction { time } { address } positions pool =
    if pool.maturity |> Maturity.isActive time |> not then
        positions
            |> Positions.toClaimTransaction pool
            |> Maybe.map
                (\claimsIn ->
                    { pool = pool
                    , to = address
                    , claimsIn = claimsIn
                    }
                )

    else
        Nothing


encode : Transaction -> Value
encode { pool, to, claimsIn } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "assetTo", to |> Address.encode )
    , ( "collateralTo", to |> Address.encode )
    , ( "claimsIn"
      , [ ( "bond", claimsIn.bond |> Uint.encode )
        , ( "insurance", claimsIn.insurance |> Uint.encode )
        ]
            |> Encode.object
      )
    ]
        |> Encode.object


view :
    { msgs | withdraw : Value -> msg }
    -> { model | device : Device, time : Posix }
    -> { user | address : Address }
    -> Positions
    -> { modal | pool : Pool }
    -> Element msg
view msgs model user positions { pool } =
    toTransaction model user positions pool
        |> Maybe.map (withdrawButton msgs model)
        |> Maybe.withDefault (disabledWithdraw model)


withdrawButton :
    { msgs | withdraw : Value -> msg }
    -> { model | device : Device }
    -> Transaction
    -> Element msg
withdrawButton msgs { device } transaction =
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
                |> msgs.withdraw
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
                (text "Claim now")
        }


disabledWithdraw : { model | device : Device } -> Element msg
disabledWithdraw { device } =
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
            (text "Claim now")
        )
