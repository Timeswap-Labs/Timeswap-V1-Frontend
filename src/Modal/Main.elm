module Modal.Main exposing
    ( Effect(..)
    , Modal
    , Msg
    , initConnect
    , initMaturityList
    , initSettings
    , initTokenList
    , receiveUser
    , subscriptions
    , update
    )

import Blockchain.Main exposing (Blockchain)
import Blockchain.User.Main as User
import Data.Chains exposing (Chains)
import Data.Deadline exposing (Deadline)
import Data.ERC20 exposing (ERC20)
import Data.Pair exposing (Pair)
import Data.Pool exposing (Pool)
import Data.Slippage exposing (Slippage)
import Data.Spot exposing (Spot)
import Data.Support exposing (Support(..))
import Data.Token exposing (Token)
import Data.TokenParam exposing (TokenParam)
import Modal.Connect.Main as Connect
import Modal.MaturityList.Main as MaturityList
import Modal.Settings.Main as Settings
import Modal.TokenList.Main as TokenList


type Modal
    = Connect Connect.Modal
    | Settings Settings.Modal
    | TokenList TokenList.Modal
    | MaturityList MaturityList.Modal


type Msg
    = ConnectMsg Connect.Msg
    | SettingsMsg Settings.Msg
    | TokenListMsg TokenList.Msg
    | MaturityListMsg MaturityList.Msg


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


initMaturityList : Blockchain -> Pair -> ( Modal, Cmd Msg )
initMaturityList blockchain pair =
    MaturityList.init blockchain pair
        |> Tuple.mapBoth
            MaturityList
            (Cmd.map MaturityListMsg)


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
