module Modal.Main exposing
    ( Effect(..)
    , Modal
    , Msg
    , initChainList
    , initChooseMaturity
    , initConfirm
    , initConnect
    , initMaturityList
    , initSettings
    , initTokenList
    , receiveUser
    , subscriptions
    , update
    , view
    )

import Blockchain.Main exposing (Blockchain)
import Blockchain.User.Main as User
import Data.Backdrop exposing (Backdrop)
import Data.Chains exposing (Chains)
import Data.ChosenZone exposing (ChosenZone)
import Data.Deadline exposing (Deadline)
import Data.ERC20 exposing (ERC20)
import Data.Images exposing (Images)
import Data.Offset exposing (Offset)
import Data.Pair exposing (Pair)
import Data.Pool exposing (Pool)
import Data.Slippage exposing (Slippage)
import Data.Spot exposing (Spot)
import Data.Support exposing (Support(..))
import Data.Token exposing (Token)
import Data.TokenParam exposing (TokenParam)
import Data.Wallets exposing (Wallets)
import Element
    exposing
        ( Element
        , map
        , none
        )
import Modal.ChainList.Main as ChainList
import Modal.Confirm.Main as Confirm
import Modal.Connect.Main as Connect
import Modal.InputMaturity.Main as InputMaturity
import Modal.MaturityList.Main as MaturityList
import Modal.Settings.Main as Settings
import Modal.TokenList.Main as TokenList
import Time exposing (Posix)


type Modal
    = Connect Connect.Modal
    | Settings Settings.Modal
    | ChainList
    | TokenList TokenList.Modal
    | MaturityList MaturityList.Modal
    | InputMaturity InputMaturity.Modal
    | Confirm Confirm.Modal


type Msg
    = ConnectMsg Connect.Msg
    | SettingsMsg Settings.Msg
    | ChainListMsg ChainList.Msg
    | TokenListMsg TokenList.Msg
    | MaturityListMsg MaturityList.Msg
    | InputMaturityMsg InputMaturity.Msg
    | ConfirmMsg Confirm.Msg


type Effect
    = UpdateSettings Slippage Deadline Spot
    | InputToken TokenParam Token
    | AddERC20 TokenParam ERC20
    | RemoveERC20 ERC20
    | RemoveAll
    | InputPool Pool


initConnect : Modal
initConnect =
    Connect.init
        |> Connect


initSettings :
    { model
        | slippage : Slippage
        , deadline : Deadline
        , spot : Spot
    }
    -> Modal
initSettings model =
    Settings.init model
        |> Settings


initTokenList : TokenParam -> Modal
initTokenList tokenParam =
    TokenList.init tokenParam
        |> TokenList


initConfirm : Modal
initConfirm =
    Confirm.init
        |> Confirm


initMaturityList :
    Blockchain
    -> Pair
    -> ( Modal, Cmd Msg )
initMaturityList blockchain pair =
    MaturityList.init blockchain pair
        |> Tuple.mapBoth
            MaturityList
            (Cmd.map MaturityListMsg)


initChooseMaturity : Pair -> Modal
initChooseMaturity pair =
    InputMaturity.init pair
        |> InputMaturity


initChainList : Modal
initChainList =
    ChainList


update :
    { model
        | chains : Chains
        , blockchain : Support User.NotSupported Blockchain
    }
    -> Msg
    -> Modal
    -> ( Maybe Modal, Cmd Msg, Maybe Effect )
update model msg modal =
    case ( msg, modal, model.blockchain ) of
        ( ConnectMsg connectMsg, Connect connect, _ ) ->
            connect
                |> Connect.update connectMsg
                |> (\( updated, cmd ) ->
                        ( updated |> Maybe.map Connect
                        , cmd |> Cmd.map ConnectMsg
                        , Nothing
                        )
                   )

        ( SettingsMsg settingsMsg, Settings settings, _ ) ->
            settings
                |> Settings.update settingsMsg
                |> (\( updated, cmd, maybeEffect ) ->
                        ( updated |> Maybe.map Settings
                        , cmd |> Cmd.map SettingsMsg
                        , maybeEffect |> Maybe.map settingsEffect
                        )
                   )

        ( ChainListMsg chainListMsg, ChainList, _ ) ->
            ChainList.update chainListMsg
                |> (\( updated, cmd ) ->
                        ( updated |> Maybe.map never
                        , cmd |> Cmd.map ChainListMsg
                        , Nothing
                        )
                   )

        ( TokenListMsg tokenListMsg, TokenList tokenList, Supported blockchain ) ->
            tokenList
                |> TokenList.update model blockchain tokenListMsg
                |> (\( updated, cmd, maybeEffect ) ->
                        ( updated |> Maybe.map TokenList
                        , cmd |> Cmd.map TokenListMsg
                        , maybeEffect |> Maybe.map tokenListEffect
                        )
                   )

        ( MaturityListMsg maturityListMsg, MaturityList maturityList, Supported blockchain ) ->
            maturityList
                |> MaturityList.update blockchain maturityListMsg
                |> (\( updated, cmd, maybeEffect ) ->
                        ( updated |> Maybe.map MaturityList
                        , cmd |> Cmd.map MaturityListMsg
                        , maybeEffect |> Maybe.map maturityListEffect
                        )
                   )

        ( InputMaturityMsg inputMaturityMsg, InputMaturity inputMaturity, Supported _ ) ->
            inputMaturity
                |> InputMaturity.update inputMaturityMsg
                |> (\updated ->
                        ( updated |> Maybe.map InputMaturity
                        , Cmd.none
                        , Nothing
                        )
                   )

        ( ConfirmMsg confirmMsg, Confirm confirm, Supported _ ) ->
            confirm
                |> Confirm.update confirmMsg
                |> (\updated ->
                        ( updated |> Maybe.map Confirm
                        , Cmd.none
                        , Nothing
                        )
                   )

        _ ->
            ( modal |> Just
            , Cmd.none
            , Nothing
            )


settingsEffect : Settings.Effect -> Effect
settingsEffect effect =
    case effect of
        Settings.UpdateSettings slippage deadline oracle ->
            UpdateSettings slippage deadline oracle


tokenListEffect : TokenList.Effect -> Effect
tokenListEffect effect =
    case effect of
        TokenList.InputToken tokenParam token ->
            InputToken tokenParam token

        TokenList.AddERC20 tokenParam erc20 ->
            AddERC20 tokenParam erc20

        TokenList.RemoveERC20 erc20 ->
            RemoveERC20 erc20

        TokenList.RemoveAll ->
            RemoveAll


maturityListEffect : MaturityList.Effect -> Effect
maturityListEffect effect =
    case effect of
        MaturityList.InputPool pool ->
            InputPool pool


receiveUser : Modal -> Maybe Modal
receiveUser modal =
    case modal of
        Connect connect ->
            connect
                |> Connect.receiveUser
                |> Maybe.map Connect

        _ ->
            Just modal


subscriptions : Modal -> Sub Msg
subscriptions modal =
    case modal of
        Connect connect ->
            connect
                |> Connect.subscriptions
                |> Sub.map ConnectMsg

        _ ->
            Sub.none


view :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , backdrop : Backdrop
        , images : Images
        , wallets : Wallets
        , chains : Chains
        , blockchain : Support User.NotSupported Blockchain
    }
    -> Modal
    -> Element Msg
view model modal =
    case modal of
        Connect connect ->
            Connect.view model connect
                |> map ConnectMsg

        Settings settings ->
            Settings.view model settings
                |> map SettingsMsg

        ChainList ->
            ChainList.view model
                |> map ChainListMsg

        TokenList tokenList ->
            case model.blockchain of
                Supported blockchain ->
                    TokenList.view model blockchain tokenList
                        |> map TokenListMsg

                _ ->
                    none

        MaturityList maturityList ->
            case model.blockchain of
                Supported _ ->
                    MaturityList.view model maturityList
                        |> map MaturityListMsg

                _ ->
                    none

        InputMaturity inputMaturity ->
            case model.blockchain of
                Supported _ ->
                    InputMaturity.view model inputMaturity
                        |> map InputMaturityMsg

                _ ->
                    none

        _ ->
            none