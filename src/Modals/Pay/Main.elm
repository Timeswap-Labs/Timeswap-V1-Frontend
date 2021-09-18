port module Modals.Pay.Main exposing
    ( Flags
    , Modal
    , Msg
    , fromFragment
    , getFlags
    , same
    , subscriptions
    , update
    , updatePayDue
    , view
    )

import Browser.Navigation as Navigation exposing (Key)
import Data.Address exposing (Address)
import Data.Allowances exposing (Allowances)
import Data.Backdrop exposing (Backdrop)
import Data.Balances as Balances exposing (Balances)
import Data.Deadline exposing (Deadline)
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Maturity as Maturity
import Data.Pair as Pair exposing (Pair)
import Data.Pool exposing (Pool)
import Data.Pools as Pools exposing (Pools)
import Data.Positions as Positions exposing (Positions)
import Data.Remote exposing (Remote(..))
import Data.Token as Token
import Data.TokenId as TokenId exposing (TokenId)
import Data.TokenImages exposing (TokenImages)
import Data.Tokens exposing (Tokens)
import Element
    exposing
        ( Element
        , alignBottom
        , alignLeft
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , inFront
        , none
        , paddingXY
        , px
        , shrink
        , spacing
        , text
        , width
        )
import Element.Font as Font
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Modals.Pay.DuesIn as DuesIn exposing (DuesIn)
import Modals.Pay.Query as Query
import Modals.Pay.Transaction as Transaction
import Page exposing (Page)
import Sort.Set exposing (Set)
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Exit as Exit
import Utility.Glass as Glass
import Utility.Loading as Loading


type Modal
    = Modal
        { pool : Pool
        , tokenIds : Set TokenId
        , duesIn : DuesIn
        }


type alias Flags =
    { pool : Pool
    , tokenIds : Set TokenId
    }


init : Positions -> Flags -> ( Modal, Cmd Msg )
init positions { pool, tokenIds } =
    ( { pool = pool
      , tokenIds = tokenIds
      , duesIn = DuesIn.init
      }
        |> Modal
    , Query.givenTokenIds positions pool tokenIds
        |> Maybe.map queryPay
        |> Maybe.withDefault Cmd.none
    )


fromFragment :
    { model
        | time : Posix
        , tokens : Tokens
        , pools : Pools
    }
    -> Positions
    -> String
    -> Maybe ( Modal, Cmd Msg )
fromFragment { time, tokens, pools } positions string =
    string
        |> String.split "&"
        |> (\list ->
                case list of
                    asset :: collateral :: maturity :: tokenIds :: _ ->
                        Maybe.map2 Flags
                            ([ asset
                             , collateral
                             , maturity
                             ]
                                |> String.join "&"
                                |> Pools.fromPoolFragment tokens pools
                            )
                            (tokenIds |> TokenId.fromFragment)
                            |> Maybe.andThen
                                (\({ pool } as flags) ->
                                    if pool.maturity |> Maturity.isActive time then
                                        Just flags

                                    else
                                        Nothing
                                )
                            |> Maybe.map (init positions)

                    _ ->
                        Nothing
           )


same : Modal -> Modal -> Bool
same (Modal modal1) (Modal modal2) =
    modal1.pool == modal2.pool && modal1.tokenIds == modal2.tokenIds


getFlags : Modal -> Flags
getFlags (Modal { pool, tokenIds }) =
    { pool = pool
    , tokenIds = tokenIds
    }


type Msg
    = ApprovePay Value
    | Pay Value
    | ReceiveTime Posix
    | SdkPayMsg Value


type alias Msgs =
    { approvePay : Value -> Msg
    , pay : Value -> Msg
    }


update :
    { model
        | key : Key
        , tokens : Tokens
        , pools : Pools
        , page : Page
    }
    -> Positions
    -> Msg
    -> Modal
    -> ( Modal, Cmd Msg )
update { key, tokens, pools, page } positions msg (Modal modal) =
    case msg of
        ApprovePay value ->
            ( Modal modal, approvePay value )

        Pay value ->
            ( Modal modal
            , Cmd.batch
                [ pay value
                , page
                    |> Page.toUrl
                    |> Navigation.pushUrl key
                ]
            )

        ReceiveTime posix ->
            ( Modal modal
            , if modal.pool.maturity |> Maturity.isActive posix then
                Cmd.none

              else
                page
                    |> Page.toUrl
                    |> Navigation.pushUrl key
            )

        SdkPayMsg value ->
            ( value
                |> Decode.decodeValue (Query.decoder pools tokens)
                |> (\result ->
                        case result of
                            Ok { pool, dues, totalDues } ->
                                if
                                    (modal.pool == pool)
                                        && (dues |> Positions.correctQuery pool positions)
                                then
                                    { modal
                                        | duesIn =
                                            Query.updateQuery modal totalDues
                                    }
                                        |> Modal

                                else
                                    Modal modal

                            Err _ ->
                                Modal modal
                   )
            , Cmd.none
            )


updatePayDue : Positions -> Modal -> Cmd Msg
updatePayDue positions (Modal { pool, tokenIds }) =
    Query.givenTokenIds positions pool tokenIds
        |> Maybe.map queryPay
        |> Maybe.withDefault Cmd.none


msgs : Msgs
msgs =
    { approvePay = ApprovePay
    , pay = Pay
    }


port queryPay : Value -> Cmd msg


port approvePay : Value -> Cmd msg


port pay : Value -> Cmd msg


port sdkPayMsg : (Value -> msg) -> Sub msg


subscriptions : { model | time : Posix } -> Modal -> Sub Msg
subscriptions { time } (Modal { pool }) =
    if pool.maturity |> Maturity.isActive time then
        sdkPayMsg SdkPayMsg

    else
        Sub.none


view :
    { model
        | device : Device
        , time : Posix
        , backdrop : Backdrop
        , deadline : Deadline
        , images : Images
        , tokenImages : TokenImages
    }
    ->
        { user
            | address : Address
            , balances : Remote () Balances
            , allowances : Remote () Allowances
        }
    -> Positions
    -> Modal
    -> Element Msg
view ({ device, backdrop, images } as model) user positions (Modal modal) =
    column
        ([ paddingXY 32 20
         , spacing 20
         , centerX
         , centerY
         , Exit.button images |> inFront
         ]
            ++ Glass.lightPrimaryModal backdrop 0
            ++ (if Device.isPhone device then
                    [ width fill
                    , height shrink
                    , alignBottom
                    ]

                else
                    [ width <| px 533
                    , height shrink
                    ]
               )
        )
        [ title
        , balance user modal
        , DuesIn.view model modal
        , Transaction.view msgs model user positions modal
        ]


title : Element msg
title =
    el
        [ width shrink
        , height shrink
        , paddingXY 0 4
        , Font.bold
        , Font.size 24
        , Font.color Color.light100
        ]
        (text "Unlock collateral")


balance :
    { user | balances : Remote () Balances }
    -> { modal | pool : { pool | pair : Pair } }
    -> Element msg
balance { balances } { pool } =
    case balances of
        Loading ->
            el
                [ height <| px 20
                , alignLeft
                ]
                Loading.view

        Failure _ ->
            none

        Success successBalances ->
            el
                [ width shrink
                , height <| px 20
                , alignLeft
                , paddingXY 0 3
                , Font.bold
                , Font.size 14
                , Font.color Color.transparent300
                ]
                ([ "Your Balance:"
                 , successBalances
                    |> Balances.get (pool.pair |> Pair.toAsset)
                 , pool.pair
                    |> Pair.toAsset
                    |> Token.toSymbol
                 ]
                    |> String.join " "
                    |> text
                )
