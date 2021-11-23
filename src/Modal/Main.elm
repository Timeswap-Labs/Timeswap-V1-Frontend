module Modal.Main exposing
    ( Effect(..)
    , Modal
    , Msg
    , closeConnect
    , initConnect
    , initSettings
    , initTokenList
    , subscriptions
    , update
    )

import Data.Deadline exposing (Deadline)
import Data.ERC20 exposing (ERC20)
import Data.Oracle exposing (Oracle)
import Data.Slippage exposing (Slippage)
import Data.Token exposing (Token)
import Data.TokenParam exposing (TokenParam)
import Modal.Connect.Main as Connect
import Modal.Settings.Main as Settings
import Modal.TokenList.Main as TokenList


type Modal
    = Connect Connect.Modal
    | Settings Settings.Modal
    | TokenList TokenList.Modal


type Msg
    = ConnectMsg Connect.Msg
    | SettingsMsg Settings.Msg
    | TokenListMsg TokenList.Msg


type Effect
    = UpdateSettings Slippage Deadline Oracle
    | InputToken TokenParam Token
    | AddERC20 TokenParam ERC20
    | RemoveERC20 ERC20
    | RemoveAll


initConnect : Modal
initConnect =
    Connect.init
        |> Connect


initSettings :
    { model
        | slippage : Slippage
        , deadline : Deadline
        , oracle : Oracle
    }
    -> Modal
initSettings model =
    Settings.init model
        |> Settings


initTokenList : TokenParam -> Modal
initTokenList tokenParam =
    TokenList.init tokenParam
        |> TokenList


update : Msg -> Modal -> ( Maybe Modal, Cmd Msg, Maybe Effect )
update msg modal =
    case ( msg, modal ) of
        ( ConnectMsg connectMsg, Connect connect ) ->
            connect
                |> Connect.update connectMsg
                |> (\( updated, cmd ) ->
                        ( updated |> Maybe.map Connect
                        , cmd |> Cmd.map ConnectMsg
                        , Nothing
                        )
                   )

        ( SettingsMsg settingsMsg, Settings settings ) ->
            settings
                |> Settings.update settingsMsg
                |> (\( updated, cmd, maybeEffect ) ->
                        ( updated |> Maybe.map Settings
                        , cmd |> Cmd.map SettingsMsg
                        , maybeEffect
                            |> Maybe.map
                                (\effect ->
                                    case effect of
                                        Settings.UpdateSettings slippage deadline oracle ->
                                            UpdateSettings slippage deadline oracle
                                )
                        )
                   )

        ( TokenListMsg tokenListMsg, TokenList tokenList ) ->
            tokenList
                |> TokenList.update tokenListMsg
                |> (\( updated, cmd, maybeEffect ) ->
                        ( updated |> Maybe.map TokenList
                        , cmd |> Cmd.map TokenListMsg
                        , maybeEffect
                            |> Maybe.map
                                (\effect ->
                                    case effect of
                                        TokenList.InputToken tokenParam token ->
                                            InputToken tokenParam token

                                        TokenList.AddERC20 tokenParam erc20 ->
                                            AddERC20 tokenParam erc20

                                        TokenList.RemoveERC20 erc20 ->
                                            RemoveERC20 erc20

                                        TokenList.RemoveAll ->
                                            RemoveAll
                                )
                        )
                   )

        _ ->
            ( modal |> Just
            , Cmd.none
            , Nothing
            )


closeConnect : Modal -> Maybe Modal
closeConnect modal =
    case modal of
        Connect connect ->
            connect
                |> Connect.close
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
