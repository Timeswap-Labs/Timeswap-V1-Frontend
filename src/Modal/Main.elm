module Modal.Main exposing
    ( Effect(..)
    , Modal
    , Msg
    , confirmed
    , initCaution
    , initCautionLiq
    , initChainList
    , initConfirm
    , initConnect
    , initMaturityList
    , initMaturityPicker
    , initPayTransaction
    , initSettings
    , initTokenList
    , receiveUser
    , reject
    , submit
    , subscriptions
    , update
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User
import Blockchain.User.TokenId exposing (TokenId)
import Blockchain.User.Txns.TxnWrite exposing (TxnWrite)
import Blockchain.User.WriteLend exposing (WriteLend)
import Blockchain.User.WriteLiquidity exposing (WriteLiquidity)
import Blockchain.User.WritePay exposing (WritePay)
import Data.Backdrop exposing (Backdrop)
import Data.CDP exposing (CDP)
import Data.Chain exposing (Chain)
import Data.Chains exposing (Chains)
import Data.ChosenZone exposing (ChosenZone)
import Data.Deadline exposing (Deadline)
import Data.Device exposing (Device)
import Data.ERC20 exposing (ERC20)
import Data.Hash exposing (Hash)
import Data.Images exposing (Images)
import Data.Offset exposing (Offset)
import Data.Pair exposing (Pair)
import Data.Pool exposing (Pool)
import Data.PriceFeed exposing (PriceFeed)
import Data.Slippage exposing (Slippage)
import Data.Support exposing (Support(..))
import Data.Theme exposing (Theme)
import Data.Token exposing (Token)
import Data.TokenParam exposing (TokenParam)
import Data.Wallets exposing (Wallets)
import Element
    exposing
        ( Element
        , map
        , none
        )
import Modal.Caution.Main as Caution
import Modal.CautionLiquidity.Main as CautionLiq exposing (Effect(..))
import Modal.ChainList.Main as ChainList
import Modal.Confirm.Main as Confirm
import Modal.Connect.Main as Connect
import Modal.MaturityList.Main as MaturityList
import Modal.MaturityPicker.Main as MaturityPicker
import Modal.PayTransaction.Main as PayTransaction
import Modal.Settings.Main as Settings
import Modal.TokenList.Main as TokenList
import Page.PoolInfo exposing (PoolInfo)
import Sort.Set exposing (Set)
import Time exposing (Posix)


type Modal
    = Connect Connect.Modal
    | Settings Settings.Modal
    | ChainList
    | TokenList TokenList.Modal
    | MaturityList MaturityList.Modal
    | MaturityPicker MaturityPicker.Modal
    | PayTransaction PayTransaction.Modal
    | Confirm Confirm.Modal
    | Caution Caution.Modal
    | CautionLiq CautionLiq.Modal


type Msg
    = ConnectMsg Connect.Msg
    | SettingsMsg Settings.Msg
    | ChainListMsg ChainList.Msg
    | TokenListMsg TokenList.Msg
    | MaturityListMsg MaturityList.Msg
    | MaturityPickerMsg MaturityPicker.Msg
    | PayTransactionMsg PayTransaction.Msg
    | ConfirmMsg Confirm.Msg
    | CautionMsg Caution.Msg
    | CautionLiqMsg CautionLiq.Msg


type Effect
    = UpdateSlippage Slippage
    | UpdateDeadline Deadline
    | UpdatePriceFeed PriceFeed
    | InputToken TokenParam Token
    | AddERC20 TokenParam ERC20
    | RemoveERC20 ERC20
    | RemoveAll
    | InputPool Pool
    | InputMaturity Pool
    | ClearTxns
    | ChangeChain Chain
    | Approve ERC20
    | Pay WritePay
    | Lend WriteLend
    | ApproveAndLend WriteLend
    | AddLiquidity WriteLiquidity


initConnect : Support User.NotSupported Blockchain -> Modal
initConnect blockchain =
    Connect.init blockchain
        |> Connect


initSettings :
    { model
        | slippage : Slippage
        , deadline : Deadline
        , priceFeed : PriceFeed
    }
    -> Modal
initSettings model =
    Settings.init model
        |> Settings


initTokenList : TokenParam -> Modal
initTokenList tokenParam =
    TokenList.init tokenParam
        |> TokenList


initConfirm : Int -> TxnWrite -> Modal
initConfirm id txnWrite =
    Confirm.init id txnWrite
        |> Confirm


initCaution : WriteLend -> Float -> CDP -> PoolInfo -> Bool -> Modal
initCaution writeLend apr cdp poolInfo isAssetApproved =
    Caution.init writeLend apr cdp poolInfo isAssetApproved
        |> Caution


initCautionLiq : WriteLiquidity -> Modal
initCautionLiq writeLiq =
    CautionLiq.init writeLiq
        |> CautionLiq


submit : Int -> Hash -> Modal -> Modal
submit id hash modal =
    case modal of
        Confirm confirmModal ->
            confirmModal
                |> Confirm.submit id hash
                |> Confirm

        _ ->
            modal


reject : Int -> Modal -> Modal
reject id modal =
    case modal of
        Confirm confirmModal ->
            confirmModal
                |> Confirm.reject id
                |> Confirm

        _ ->
            modal


confirmed : Hash -> Modal -> Modal
confirmed hash modal =
    case modal of
        Confirm confirmModal ->
            confirmModal
                |> Confirm.confirmed hash
                |> Confirm

        _ ->
            modal


initMaturityList :
    Blockchain
    -> String
    -> Pair
    -> ( Modal, Cmd Msg )
initMaturityList blockchain endPoint pair =
    MaturityList.init blockchain endPoint pair
        |> Tuple.mapBoth
            MaturityList
            (Cmd.map MaturityListMsg)


initMaturityPicker : Pair -> Posix -> ( Modal, Cmd Msg )
initMaturityPicker pair time =
    MaturityPicker.init pair time
        |> Tuple.mapBoth
            MaturityPicker
            (Cmd.map MaturityPickerMsg)


initChainList : Modal
initChainList =
    ChainList


initPayTransaction :
    Pool
    -> Set TokenId
    -> ( Modal, Cmd Msg )
initPayTransaction pool set =
    PayTransaction.init pool set
        |> Tuple.mapBoth
            PayTransaction
            (Cmd.map PayTransactionMsg)


update :
    { model
        | chains : Chains
        , blockchain : Support User.NotSupported Blockchain
        , time : Posix
        , endPoint : String
        , offset : Offset
    }
    -> Msg
    -> Modal
    -> ( Maybe Modal, Cmd Msg, Maybe Effect )
update model msg modal =
    case ( msg, modal, model.blockchain ) of
        ( ConnectMsg connectMsg, Connect connect, _ ) ->
            connect
                |> Connect.update connectMsg
                |> (\( updated, cmd, maybeEffect ) ->
                        ( updated |> Maybe.map Connect
                        , cmd |> Cmd.map ConnectMsg
                        , maybeEffect |> Maybe.map connectEffect
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
            ChainList.update model.blockchain chainListMsg
                |> (\( updated, cmd, maybeEffect ) ->
                        ( updated |> Maybe.map (\_ -> ChainList)
                        , cmd |> Cmd.map ChainListMsg
                        , maybeEffect |> Maybe.map chainListEffect
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
                |> MaturityList.update model blockchain maturityListMsg
                |> (\( updated, cmd, maybeEffect ) ->
                        ( updated |> Maybe.map MaturityList
                        , cmd |> Cmd.map MaturityListMsg
                        , maybeEffect |> Maybe.map maturityListEffect
                        )
                   )

        ( MaturityPickerMsg inputMaturityMsg, MaturityPicker inputMaturity, Supported _ ) ->
            inputMaturity
                |> MaturityPicker.update model inputMaturityMsg
                |> (\( updated, maybeEffect ) ->
                        ( updated |> Maybe.map MaturityPicker
                        , Cmd.none
                        , maybeEffect |> Maybe.map maturityPickerEffect
                        )
                   )

        ( PayTransactionMsg payTransactionMsg, PayTransaction payTransaction, Supported blockchain ) ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.map
                    (\user ->
                        payTransaction
                            |> PayTransaction.update user payTransactionMsg
                            |> (\( updated, cmd, maybeEffect ) ->
                                    ( updated |> Maybe.map PayTransaction
                                    , cmd |> Cmd.map PayTransactionMsg
                                    , maybeEffect |> Maybe.map payTransactionEffect
                                    )
                               )
                    )
                |> Maybe.withDefault
                    ( modal |> Just
                    , Cmd.none
                    , Nothing
                    )

        ( ConfirmMsg confirmMsg, Confirm _, Supported _ ) ->
            Confirm.update confirmMsg
                |> (\updated ->
                        ( updated |> Maybe.map never
                        , Cmd.none
                        , Nothing
                        )
                   )

        ( CautionMsg cautionMsg, Caution cautionModal, Supported _ ) ->
            Caution.update cautionMsg cautionModal
                |> (\( updated, maybeEffect ) ->
                        ( updated |> Maybe.map Caution
                        , Cmd.none
                        , maybeEffect |> Maybe.map cautionLendEffect
                        )
                   )

        ( CautionLiqMsg cautionLiqMsg, CautionLiq cautionLiqModal, Supported _ ) ->
            CautionLiq.update cautionLiqMsg cautionLiqModal
                |> (\( updated, maybeEffect ) ->
                        ( updated |> Maybe.map CautionLiq
                        , Cmd.none
                        , maybeEffect |> Maybe.map cautionLiqEffect
                        )
                   )

        _ ->
            ( modal |> Just
            , Cmd.none
            , Nothing
            )


connectEffect : Connect.Effect -> Effect
connectEffect effect =
    case effect of
        Connect.ClearTxns ->
            ClearTxns


settingsEffect : Settings.Effect -> Effect
settingsEffect effect =
    case effect of
        Settings.UpdateSlippage slippage ->
            UpdateSlippage slippage

        Settings.UpdateDeadline deadline ->
            UpdateDeadline deadline

        Settings.UpdatePriceFeed priceFeed ->
            UpdatePriceFeed priceFeed


chainListEffect : ChainList.Effect -> Effect
chainListEffect effect =
    case effect of
        ChainList.ChangeChain chain ->
            ChangeChain chain


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


maturityPickerEffect : MaturityPicker.Effect -> Effect
maturityPickerEffect effect =
    case effect of
        MaturityPicker.InputMaturity pool ->
            InputMaturity pool


payTransactionEffect : PayTransaction.Effect -> Effect
payTransactionEffect effect =
    case effect of
        PayTransaction.Approve erc20 ->
            Approve erc20

        PayTransaction.Pay writePay ->
            Pay writePay


cautionLendEffect : Caution.Effect -> Effect
cautionLendEffect effect =
    case effect of
        Caution.Lend writeLend ->
            Lend writeLend

        Caution.ApproveAndLend writeLend ->
            ApproveAndLend writeLend


cautionLiqEffect : CautionLiq.Effect -> Effect
cautionLiqEffect effect =
    case effect of
        CautionLiq.AddLiquidity writeLiq ->
            AddLiquidity writeLiq


receiveUser : Modal -> Maybe Modal
receiveUser modal =
    case modal of
        Connect connect ->
            connect
                |> Connect.receiveUser
                |> Maybe.map Connect

        _ ->
            Just modal


subscriptions : { model | offset : Offset } -> Modal -> Sub Msg
subscriptions model modal =
    case modal of
        Connect connect ->
            connect
                |> Connect.subscriptions
                |> Sub.map ConnectMsg

        Settings settings ->
            settings
                |> Settings.subscriptions
                |> Sub.map SettingsMsg

        MaturityList maturityList ->
            maturityList
                |> MaturityList.subscriptions
                |> Sub.map MaturityListMsg

        MaturityPicker maturityPicker ->
            maturityPicker
                |> MaturityPicker.subscriptions model.offset
                |> Sub.map MaturityPickerMsg

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
        , device : Device
        , priceFeed : PriceFeed
        , theme : Theme
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

        MaturityPicker inputMaturity ->
            case model.blockchain of
                Supported _ ->
                    MaturityPicker.view model inputMaturity
                        |> map MaturityPickerMsg

                _ ->
                    none

        PayTransaction payTransaction ->
            case model.blockchain of
                Supported blockchain ->
                    blockchain
                        |> Blockchain.toUser
                        |> Maybe.map
                            (\user ->
                                payTransaction
                                    |> PayTransaction.view model blockchain user
                                    |> map PayTransactionMsg
                            )
                        |> Maybe.withDefault none

                _ ->
                    none

        Confirm confirmModal ->
            case model.blockchain of
                Supported blockchain ->
                    confirmModal
                        |> Confirm.view model blockchain
                        |> map ConfirmMsg

                _ ->
                    none

        Caution cautionModal ->
            cautionModal
                |> Caution.view model
                |> map CautionMsg

        CautionLiq cautionLiqModal ->
            cautionLiqModal
                |> CautionLiq.view model
                |> map CautionLiqMsg
