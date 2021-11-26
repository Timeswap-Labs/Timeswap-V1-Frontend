port module Modal.Connect.Main exposing
    ( Modal
    , Msg
    , init
    , receiveUser
    , subscriptions
    , update
    , view
    )

import Data.Address as Address exposing (Address)
import Data.Remote exposing (Remote(..))
import Data.Wallet as Wallet exposing (Wallet)
import Element
    exposing
        ( Element
        , alpha
        , behindContent
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , none
        , px
        , width
        )
import Element.Background as Background
import Element.Events as Events
import Element.Input as Input
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Modal.Connect.Error as Error exposing (Error)
import Utility.Color as Color


type Modal
    = Wallets
    | Waiting
        { wallet : Wallet
        , error : Remote Error Never
        }
    | Connected


type Msg
    = GoToWallets
    | Connect Wallet
    | TryAgain
    | CopyToClipboard Address
    | ReceiveNoConnect Value
    | Exit


init : Modal
init =
    Wallets


update : Msg -> Modal -> ( Maybe Modal, Cmd Msg )
update msg modal =
    case ( msg, modal ) of
        ( GoToWallets, Waiting _ ) ->
            ( Wallets |> Just
            , Cmd.none
            )

        ( GoToWallets, Connected ) ->
            ( Wallets |> Just
            , Cmd.none
            )

        ( Connect wallet, Wallets ) ->
            ( { wallet = wallet
              , error = Loading
              }
                |> Waiting
                |> Just
            , wallet
                |> Wallet.encode
                |> connect
            )

        ( TryAgain, Waiting waiting ) ->
            ( { waiting | error = Loading }
                |> Waiting
                |> Just
            , waiting.wallet
                |> Wallet.encode
                |> connect
            )

        ( CopyToClipboard address, _ ) ->
            ( modal |> Just
            , address
                |> Address.encode
                |> copyToClipboard
            )

        ( ReceiveNoConnect value, Waiting waiting ) ->
            ( (case
                value
                    |> Decode.decodeValue Wallet.decoder
               of
                Ok wallet ->
                    if wallet == waiting.wallet then
                        { waiting | error = Error.NoConnect |> Failure }
                            |> Waiting

                    else
                        modal

                Err _ ->
                    modal
              )
                |> Just
            , Cmd.none
            )

        ( Exit, _ ) ->
            ( Nothing
            , Cmd.none
            )

        _ ->
            ( modal |> Just
            , Cmd.none
            )


receiveUser : Modal -> Maybe Modal
receiveUser modal =
    case modal of
        Connected ->
            Just modal

        _ ->
            Nothing


port connect : Value -> Cmd msg


port copyToClipboard : Value -> Cmd msg


port receiveNoConnect : (Value -> msg) -> Sub msg


subscriptions : Modal -> Sub Msg
subscriptions modal =
    case modal of
        Waiting { error } ->
            case error of
                Loading ->
                    receiveNoConnect ReceiveNoConnect

                _ ->
                    Sub.none

        _ ->
            Sub.none


view : Modal -> Element Msg
view modal =
    el
        [ width fill
        , height fill
        , el
            [ width fill
            , height fill
            , Background.color Color.dark500
            , alpha 0.1
            , Events.onClick Exit
            ]
            none
            |> behindContent
        ]
        (column
            [ width <| px 335
            , height <| px 300
            , centerX
            , centerY
            , Background.color Color.light100
            ]
            []
        )
