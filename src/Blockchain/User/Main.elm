port module Blockchain.User.Main exposing
    ( Effect(..)
    , Flag
    , Msg
    , NotSupported
    , User
    , getAllowance
    , getBalance
    , getClaims
    , getDues
    , getLiqs
    , getPoolNatives
    , hasEnoughAllowance
    , hasEnoughBalance
    , init
    , initNotSupported
    , isApprovePending
    , receiveNotSupported
    , receiveUser
    , receiveUserInit
    , subscriptions
    , toAddress
    , toAddressNotSupported
    , toName
    , toPendingSize
    , toTxnsList
    , toWallet
    , toWalletNotSupported
    , update
    , updateAddERC20
    , updateApprove
    , updateBorrow
    , updateBurn
    , updateClearTxns
    , updateCreate
    , updateLend
    , updateLiquidity
    , updatePay
    , updateWithdraw
    )

import Blockchain.User.Allowances as Allowances exposing (Allowances)
import Blockchain.User.Balances as Balances exposing (Balances)
import Blockchain.User.Cache as Cache
import Blockchain.User.Claims exposing (Claims)
import Blockchain.User.Dues exposing (Dues)
import Blockchain.User.Liqs exposing (Liqs)
import Blockchain.User.Natives as Natives exposing (Natives)
import Blockchain.User.Positions as Positions exposing (Positions)
import Blockchain.User.Txns.Main as Txns exposing (Txns)
import Blockchain.User.Txns.Receipt as Receipt
import Blockchain.User.Txns.Txn as Txn exposing (Txn)
import Blockchain.User.Txns.TxnWrite as TxnWrite exposing (TxnWrite)
import Blockchain.User.Write as Write
import Blockchain.User.WriteApprove as WriteApprove
import Blockchain.User.WriteBorrow as WriteBorrow exposing (WriteBorrow)
import Blockchain.User.WriteBurn as WriteBurn exposing (WriteBurn)
import Blockchain.User.WriteCreate as WriteCreate exposing (WriteCreate)
import Blockchain.User.WriteLend as WriteLend exposing (WriteLend)
import Blockchain.User.WriteLiquidity as WriteLiquidity exposing (WriteLiquidity)
import Blockchain.User.WritePay as WritePay exposing (WritePay)
import Blockchain.User.WriteWithdraw as WriteWithdraw exposing (WriteWithdraw)
import Data.Address as Address exposing (Address)
import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.Deadline exposing (Deadline)
import Data.ERC20 as ERC20 exposing (ERC20)
import Data.ERC20s exposing (ERC20s)
import Data.Hash exposing (Hash)
import Data.Pool exposing (Pool)
import Data.Remote as Remote exposing (Remote(..))
import Data.Token as Token exposing (Token)
import Data.Uint exposing (Uint)
import Data.Wallet as Wallet exposing (Wallet)
import Data.Web exposing (Web)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode exposing (Value)
import Process
import Sort.Dict as Dict exposing (Dict)
import Sort.Set as Set
import Task
import Time exposing (Posix)


type User
    = User
        { wallet : Wallet
        , address : Address
        , name : Maybe String
        , balances : Balances
        , allowances : Allowances
        , positions : Web Positions
        , txns : Txns
        , natives : Web (Dict Pool Natives)
        }


type NotSupported
    = NotSupported
        { wallet : Wallet
        , address : Address
        }


type alias Flag =
    { chainId : Int
    , wallet : Wallet.Flag
    , address : String
    , txns : Txns.Flags
    }


type Msg
    = QueryNatives ()
    | ReceiveBalances Value
    | ReceiveAllowances Value
    | ReceiveConfirm Value
    | ReceiveReceipt Value
    | ReceiveNatives Chain (Result Http.Error Natives.Answer)
    | ReceivePositions Value
    | BalancesTick Posix
    | AllowancesTick Posix


type Effect
    = AddERC20s ERC20s
    | OpenConfirm Int TxnWrite
    | ConfirmTxn Int Hash
    | RejectTxn Int


init : Chains -> Chain -> Flag -> Maybe ( User, Cmd Msg )
init chains chain flag =
    case
        ( flag.wallet |> Wallet.init
        , flag.address |> Address.fromString
        )
    of
        ( Just wallet, Just address ) ->
            ( { wallet = wallet
              , address = address
              , name = Nothing
              , balances = Balances.init chains chain
              , allowances = Allowances.init chains chain
              , positions = Remote.loading
              , txns = flag.txns |> Txns.init
              , natives = Remote.loading
              }
                |> User
            , getNatives chain chains
            )
                |> Just

        _ ->
            Nothing


initNotSupported : Flag -> Maybe NotSupported
initNotSupported flag =
    case
        ( flag.wallet |> Wallet.init
        , flag.address |> Address.fromString
        )
    of
        ( Just wallet, Just address ) ->
            { wallet = wallet
            , address = address
            }
                |> NotSupported
                |> Just

        _ ->
            Nothing


receiveUserInit :
    { model | chains : Chains }
    -> Chain
    -> Value
    -> Maybe ( User, Cmd Msg )
receiveUserInit ({ chains } as model) chain value =
    case value |> Decode.decodeValue (decoder model chain) of
        Ok (Just (User decodedUser)) ->
            ( User decodedUser
            , [ decodedUser.address
                    |> Balances.encode chains chain
                    |> balancesOf
              , decodedUser.address
                    |> Allowances.encode chains chain
                    |> allowancesOf
              , getNatives chain chains
              ]
                |> Cmd.batch
            )
                |> Just

        _ ->
            Nothing


receiveUser :
    { model | chains : Chains }
    -> Chain
    -> Value
    -> User
    -> Maybe ( User, Cmd Msg )
receiveUser ({ chains } as model) chain value user =
    case value |> Decode.decodeValue (decoder model chain) of
        Ok (Just (User decodedUser)) ->
            if isSame user (User decodedUser) then
                ( user
                , Cmd.none
                )
                    |> Just

            else
                ( User decodedUser
                , [ decodedUser.address
                        |> Balances.encode chains chain
                        |> balancesOf
                  , decodedUser.address
                        |> Allowances.encode chains chain
                        |> allowancesOf
                  , getNatives chain chains
                  ]
                    |> Cmd.batch
                )
                    |> Just

        Ok Nothing ->
            Nothing

        Err _ ->
            ( user
            , Cmd.none
            )
                |> Just


receiveNotSupported :
    Value
    -> Maybe NotSupported
receiveNotSupported value =
    case
        value
            |> Decode.decodeValue decoderNotSupported
    of
        Ok decodedUser ->
            Just decodedUser

        Err _ ->
            Nothing


update : { model | chains : Chains } -> Chain -> Msg -> User -> ( User, Cmd Msg, Maybe Effect )
update { chains } chain msg (User user) =
    case msg of
        QueryNatives () ->
            ( user |> User
            , getNatives chain chains
            , Nothing
            )

        ReceiveBalances value ->
            (case value |> Decode.decodeValue Balances.decoder of
                Ok decoded ->
                    if
                        (decoded.chain == chain)
                            && (decoded.address == user.address)
                    then
                        { user
                            | balances =
                                user.balances
                                    |> Dict.insertAll decoded.balances
                        }

                    else
                        user

                Err _ ->
                    user
            )
                |> noCmdAndEffect

        ReceiveAllowances value ->
            (case value |> Decode.decodeValue Allowances.decoder of
                Ok decoded ->
                    if
                        (decoded.chain == chain)
                            && (decoded.address == user.address)
                    then
                        { user
                            | allowances =
                                user.allowances
                                    |> Dict.insertAll decoded.allowances
                        }

                    else
                        user

                Err _ ->
                    user
            )
                |> noCmdAndEffect

        ReceiveConfirm value ->
            case value |> Decode.decodeValue Write.decoder of
                Ok decoded ->
                    case
                        ( (decoded.chain == chain)
                            && (decoded.address == user.address)
                        , decoded.hash
                        )
                    of
                        ( True, Just hash ) ->
                            user.txns
                                |> Txns.confirm decoded.id
                                    hash
                                    Txn.Pending
                                |> (\txns ->
                                        ( { user | txns = txns }
                                            |> User
                                        , txns
                                            |> Cache.encodeTxns chain user.address
                                            |> cacheTxns
                                        , ConfirmTxn decoded.id hash |> Just
                                        )
                                   )

                        ( True, Nothing ) ->
                            ( user |> User
                            , Cmd.none
                            , RejectTxn decoded.id |> Just
                            )

                        _ ->
                            user |> noCmdAndEffect

                Err _ ->
                    user |> noCmdAndEffect

        ReceiveReceipt value ->
            (case value |> Decode.decodeValue Receipt.decoder of
                Ok decoded ->
                    if
                        (decoded.chain == chain)
                            && (decoded.address == user.address)
                            && (decoded.hash
                                    |> Set.memberOf
                                        (user.txns |> Txns.getPending)
                               )
                    then
                        { user
                            | txns =
                                case decoded.state of
                                    Receipt.Failed ->
                                        user.txns
                                            |> Txns.updateFailed decoded.hash

                                    Receipt.Success ->
                                        user.txns
                                            |> Txns.updateSuccess decoded.hash
                        }

                    else
                        user

                Err _ ->
                    user
            )
                |> noCmdAndEffect

        ReceiveNatives decodedChain (Ok natives) ->
            ( { user | natives = Remote.Success natives } |> User
            , if decodedChain == chain then
                [ natives
                    |> Natives.encode chain user.address
                    |> positionsOf
                , Process.sleep 300000
                    |> Task.perform QueryNatives
                ]
                    |> Cmd.batch

              else
                Cmd.none
            , Nothing
            )

        ReceiveNatives decodedChain (Err error) ->
            if decodedChain == chain then
                ( { user | natives = Failure error }
                    |> User
                , Process.sleep 5000
                    |> Task.perform QueryNatives
                , Nothing
                )

            else
                user |> noCmdAndEffect

        ReceivePositions value ->
            case value |> Decode.decodeValue (Positions.decoder chains) of
                Ok decoded ->
                    if
                        (decoded.chain == chain)
                            && (decoded.owner == user.address)
                    then
                        decoded.positions
                            |> Positions.toERC20s
                            |> Chains.getDoesNotExist chain chains
                            |> (\erc20s ->
                                    ( erc20s
                                        |> Set.foldl
                                            (\erc20 accumulator ->
                                                user.balances
                                                    |> Dict.get (Token.ERC20 erc20)
                                                    |> Maybe.map (\_ -> accumulator)
                                                    |> Maybe.withDefault
                                                        (accumulator
                                                            |> Set.insert (Token.ERC20 erc20)
                                                        )
                                            )
                                            (Set.empty Token.sorter)
                                    , erc20s
                                        |> Set.foldl
                                            (\erc20 accumulator ->
                                                user.allowances
                                                    |> Dict.get erc20
                                                    |> Maybe.map (\_ -> accumulator)
                                                    |> Maybe.withDefault
                                                        (accumulator
                                                            |> Set.insert erc20
                                                        )
                                            )
                                            (Set.empty ERC20.sorter)
                                    )
                               )
                            |> (\( tokens, erc20s ) ->
                                    ( { user
                                        | balances =
                                            tokens
                                                |> Set.foldl
                                                    (\token accumulator ->
                                                        accumulator
                                                            |> Dict.insert token Remote.loading
                                                    )
                                                    user.balances
                                        , allowances =
                                            erc20s
                                                |> Set.foldl
                                                    (\erc20 accumulator ->
                                                        accumulator
                                                            |> Dict.insert erc20 Remote.loading
                                                    )
                                                    user.allowances
                                        , positions =
                                            user.positions
                                                |> Remote.map
                                                    (\{ claims, dues, liqs } ->
                                                        { claims =
                                                            claims
                                                                |> Dict.insertAll decoded.positions.claims
                                                        , dues =
                                                            dues
                                                                |> Dict.insertAll decoded.positions.dues
                                                        , liqs =
                                                            liqs
                                                                |> Dict.insertAll decoded.positions.liqs
                                                        }
                                                            |> Success
                                                    )
                                                |> Remote.withDefault
                                                    (decoded.positions |> Success)
                                      }
                                        |> User
                                    , [ tokens
                                            |> Balances.encodeMultiple chain user.address
                                            |> balancesOf
                                      , erc20s
                                            |> Allowances.encodeMultiple chain user.address
                                            |> allowancesOf
                                      ]
                                        |> Cmd.batch
                                    , Nothing
                                    )
                               )

                    else
                        user |> noCmdAndEffect

                _ ->
                    user |> noCmdAndEffect

        BalancesTick posix ->
            ( { user | balances = user.balances |> Balances.update posix }
                |> User
            , Cmd.none
            , Nothing
            )

        AllowancesTick posix ->
            ( { user | allowances = user.allowances |> Allowances.update posix }
                |> User
            , Cmd.none
            , Nothing
            )


updateClearTxns : Chain -> User -> ( User, Cmd Msg )
updateClearTxns chain (User user) =
    ( { user | txns = Txns.initEmpty } |> User
    , Txns.initEmpty
        |> Cache.encodeTxns chain user.address
        |> cacheTxns
    )


updateAddERC20 : Chain -> ERC20 -> User -> ( User, Cmd Msg )
updateAddERC20 chain erc20 (User user) =
    ( user.balances |> Dict.get (Token.ERC20 erc20)
    , user.allowances |> Dict.get erc20
    )
        |> (\( balance, allowance ) ->
                ( { user
                    | balances =
                        balance
                            |> Maybe.map (\_ -> user.balances)
                            |> Maybe.withDefault
                                (user.balances
                                    |> Dict.insert (Token.ERC20 erc20) Remote.loading
                                )
                    , allowances =
                        allowance
                            |> Maybe.map (\_ -> user.allowances)
                            |> Maybe.withDefault
                                (user.allowances
                                    |> Dict.insert erc20 Remote.loading
                                )
                  }
                    |> User
                , [ balance
                        |> Maybe.map (\_ -> Cmd.none)
                        |> Maybe.withDefault
                            (Token.ERC20 erc20
                                |> Balances.encodeSingle chain user.address
                                |> balancesOf
                            )
                  , allowance
                        |> Maybe.map (\_ -> Cmd.none)
                        |> Maybe.withDefault
                            (erc20
                                |> Allowances.encodeSingle chain user.address
                                |> allowancesOf
                            )
                  ]
                    |> Cmd.batch
                )
           )


updateApprove : Chain -> ERC20 -> User -> ( User, Cmd Msg, Effect )
updateApprove chain erc20 (User user) =
    user.txns
        |> Txns.insert (TxnWrite.Approve erc20)
        |> (\( id, txns ) ->
                ( { user | txns = txns }
                    |> User
                , [ erc20
                        |> WriteApprove.encode id chain user.address
                        |> approve
                  , txns
                        |> Cache.encodeTxns chain user.address
                        |> cacheTxns
                  ]
                    |> Cmd.batch
                , OpenConfirm id (TxnWrite.Approve erc20)
                )
           )


updateLend :
    { model | time : Posix, deadline : Deadline }
    -> Chain
    -> WriteLend
    -> User
    -> ( User, Cmd Msg, Effect )
updateLend model chain writeLend (User user) =
    user.txns
        |> Txns.insert
            (TxnWrite.Lend
                (writeLend |> WriteLend.toPool)
            )
        |> (\( id, txns ) ->
                ( { user | txns = txns }
                    |> User
                , [ writeLend
                        |> WriteLend.encode model user.address
                        |> Write.encode id chain user.address
                        |> lend
                  , txns
                        |> Cache.encodeTxns chain user.address
                        |> cacheTxns
                  ]
                    |> Cmd.batch
                , OpenConfirm id
                    (TxnWrite.Lend
                        (writeLend |> WriteLend.toPool)
                    )
                )
           )


updateBorrow :
    { model | time : Posix, deadline : Deadline }
    -> Chain
    -> WriteBorrow
    -> User
    -> ( User, Cmd Msg, Effect )
updateBorrow model chain writeBorrow (User user) =
    user.txns
        |> Txns.insert
            (TxnWrite.Borrow
                (writeBorrow |> WriteBorrow.toPool)
            )
        |> (\( id, txns ) ->
                ( { user | txns = txns }
                    |> User
                , [ writeBorrow
                        |> WriteBorrow.encode model user.address
                        |> Write.encode id chain user.address
                        |> borrow
                  , txns
                        |> Cache.encodeTxns chain user.address
                        |> cacheTxns
                  ]
                    |> Cmd.batch
                , OpenConfirm id
                    (TxnWrite.Borrow
                        (writeBorrow |> WriteBorrow.toPool)
                    )
                )
           )


updateLiquidity :
    { model | time : Posix, deadline : Deadline }
    -> Chain
    -> WriteLiquidity
    -> User
    -> ( User, Cmd Msg, Effect )
updateLiquidity model chain writeLiquidity (User user) =
    user.txns
        |> Txns.insert
            (TxnWrite.Liquidity
                (writeLiquidity |> WriteLiquidity.toPool)
            )
        |> (\( id, txns ) ->
                ( { user | txns = txns }
                    |> User
                , [ writeLiquidity
                        |> WriteLiquidity.encode model user.address
                        |> Write.encode id chain user.address
                        |> liquidity
                  , txns
                        |> Cache.encodeTxns chain user.address
                        |> cacheTxns
                  ]
                    |> Cmd.batch
                , OpenConfirm id
                    (TxnWrite.Liquidity
                        (writeLiquidity |> WriteLiquidity.toPool)
                    )
                )
           )


updateCreate :
    { model | time : Posix, deadline : Deadline }
    -> Chain
    -> WriteCreate
    -> User
    -> ( User, Cmd Msg, Effect )
updateCreate model chain writeCreate (User user) =
    user.txns
        |> Txns.insert
            (TxnWrite.Create
                (writeCreate |> WriteCreate.toPool)
            )
        |> (\( id, txns ) ->
                ( { user | txns = txns }
                    |> User
                , [ writeCreate
                        |> WriteCreate.encode model user.address
                        |> Write.encode id chain user.address
                        |> create
                  , txns
                        |> Cache.encodeTxns chain user.address
                        |> cacheTxns
                  ]
                    |> Cmd.batch
                , OpenConfirm id
                    (TxnWrite.Create
                        (writeCreate |> WriteCreate.toPool)
                    )
                )
           )


updateWithdraw :
    Chain
    -> WriteWithdraw
    -> User
    -> ( User, Cmd Msg, Effect )
updateWithdraw chain writeWithdraw (User user) =
    user.txns
        |> Txns.insert
            (TxnWrite.Withdraw
                (writeWithdraw |> WriteWithdraw.toPool)
            )
        |> (\( id, txns ) ->
                ( { user | txns = txns }
                    |> User
                , [ writeWithdraw
                        |> WriteWithdraw.encode user.address
                        |> Write.encode id chain user.address
                        |> withdraw
                  , txns
                        |> Cache.encodeTxns chain user.address
                        |> cacheTxns
                  ]
                    |> Cmd.batch
                , OpenConfirm id
                    (TxnWrite.Withdraw
                        (writeWithdraw |> WriteWithdraw.toPool)
                    )
                )
           )


updatePay :
    { model | time : Posix, deadline : Deadline }
    -> Chain
    -> WritePay
    -> User
    -> ( User, Cmd Msg, Effect )
updatePay model chain writePay (User user) =
    user.txns
        |> Txns.insert
            (TxnWrite.Pay
                (writePay |> WritePay.toPool)
            )
        |> (\( id, txns ) ->
                ( { user | txns = txns }
                    |> User
                , [ writePay
                        |> WritePay.encode model user.address
                        |> Write.encode id chain user.address
                        |> pay
                  , txns
                        |> Cache.encodeTxns chain user.address
                        |> cacheTxns
                  ]
                    |> Cmd.batch
                , OpenConfirm id
                    (TxnWrite.Pay
                        (writePay |> WritePay.toPool)
                    )
                )
           )


updateBurn :
    Chain
    -> WriteBurn
    -> User
    -> ( User, Cmd Msg, Effect )
updateBurn chain writeBurn (User user) =
    user.txns
        |> Txns.insert
            (TxnWrite.Burn
                (writeBurn |> WriteBurn.toPool)
            )
        |> (\( id, txns ) ->
                ( { user | txns = txns }
                    |> User
                , [ writeBurn
                        |> WriteBurn.encode user.address
                        |> Write.encode id chain user.address
                        |> burn
                  , txns
                        |> Cache.encodeTxns chain user.address
                        |> cacheTxns
                  ]
                    |> Cmd.batch
                , OpenConfirm id
                    (TxnWrite.Burn
                        (writeBurn |> WriteBurn.toPool)
                    )
                )
           )


noCmdAndEffect :
    { wallet : Wallet
    , address : Address
    , name : Maybe String
    , balances : Balances
    , allowances : Allowances
    , positions : Web Positions
    , txns : Txns
    , natives : Web (Dict Pool Natives)
    }
    -> ( User, Cmd Msg, Maybe Effect )
noCmdAndEffect user =
    ( user |> User
    , Cmd.none
    , Nothing
    )


decoder :
    { model | chains : Chains }
    -> Chain
    -> Decoder (Maybe User)
decoder { chains } chain =
    Decode.succeed
        (\wallet address txns ->
            { wallet = wallet
            , address = address
            , name = Nothing
            , balances = Balances.init chains chain
            , allowances = Allowances.init chains chain
            , positions = Remote.loading
            , txns = txns
            , natives = Remote.loading
            }
                |> User
        )
        |> Pipeline.required "wallet" Wallet.decoder
        |> Pipeline.required "address" Address.decoder
        |> Pipeline.required "txns" Txns.decoder
        |> Decode.nullable


decoderNotSupported : Decoder NotSupported
decoderNotSupported =
    Decode.succeed
        (\wallet address ->
            { wallet = wallet
            , address = address
            }
                |> NotSupported
        )
        |> Pipeline.required "wallet" Wallet.decoder
        |> Pipeline.required "address" Address.decoder


isSame : User -> User -> Bool
isSame (User user1) (User user2) =
    (user1.address == user2.address)
        && (user1.wallet == user2.wallet)


getNatives :
    Chain
    -> Chains
    -> Cmd Msg
getNatives chain chains =
    Http.get
        { url = Natives.toUrlString chain
        , expect =
            Natives.decoder chain chains
                |> Http.expectJson
                    (ReceiveNatives chain)
        }


port cacheTxns : Value -> Cmd msg


port balancesOf : Value -> Cmd msg


port allowancesOf : Value -> Cmd msg


port positionsOf : Value -> Cmd msg


port approve : Value -> Cmd msg


port lend : Value -> Cmd msg


port borrow : Value -> Cmd msg


port liquidity : Value -> Cmd msg


port burn : Value -> Cmd msg


port create : Value -> Cmd msg


port withdraw : Value -> Cmd msg


port pay : Value -> Cmd msg


port receiveBalances : (Value -> msg) -> Sub msg


port receiveAllowances : (Value -> msg) -> Sub msg


port receivePositions : (Value -> msg) -> Sub msg


port receiveConfirm : (Value -> msg) -> Sub msg


port receiveReceipt : (Value -> msg) -> Sub msg


subscriptions : User -> Sub Msg
subscriptions (User user) =
    [ receiveBalances ReceiveBalances
    , receiveAllowances ReceiveAllowances
    , receivePositions ReceivePositions
    , receiveConfirm ReceiveConfirm
    , receiveReceipt ReceiveReceipt
    , user.balances |> Balances.subscriptions BalancesTick
    , user.allowances |> Allowances.subscriptions AllowancesTick
    ]
        |> Sub.batch


toWallet : User -> Wallet
toWallet (User { wallet }) =
    wallet


toWalletNotSupported : NotSupported -> Wallet
toWalletNotSupported (NotSupported { wallet }) =
    wallet


toAddress : User -> Address
toAddress (User { address }) =
    address


toAddressNotSupported : NotSupported -> Address
toAddressNotSupported (NotSupported { address }) =
    address


toName : User -> Maybe String
toName (User { name }) =
    name
        |> Maybe.map
            (\justName ->
                if (justName |> String.length) > 20 then
                    [ justName |> String.left 20
                    , "..."
                    ]
                        |> String.concat

                else
                    justName
            )


getBalance : Token -> User -> Maybe (Web Uint)
getBalance token (User { balances }) =
    balances
        |> Dict.get token


getAllowance : ERC20 -> User -> Maybe (Web Uint)
getAllowance erc20 (User { allowances }) =
    allowances
        |> Dict.get erc20


hasEnoughBalance : Token -> Uint -> User -> Bool
hasEnoughBalance token amount (User { balances }) =
    balances
        |> Balances.hasEnough token amount


hasEnoughAllowance : ERC20 -> Uint -> User -> Bool
hasEnoughAllowance erc20 amount (User { allowances }) =
    allowances
        |> Allowances.hasEnough erc20 amount


toPendingSize : User -> Int
toPendingSize (User { txns }) =
    txns
        |> Txns.toPendingSize


toTxnsList : User -> List ( Hash, Txn )
toTxnsList (User { txns }) =
    txns
        |> Txns.toList


isApprovePending : ERC20 -> User -> Bool
isApprovePending erc20 (User { txns }) =
    txns
        |> Txns.isPending erc20


getClaims : User -> Web Claims
getClaims (User { positions }) =
    positions
        |> Remote.map .claims


getDues : User -> Web Dues
getDues (User { positions }) =
    positions
        |> Remote.map .dues


getLiqs : User -> Web Liqs
getLiqs (User { positions }) =
    positions
        |> Remote.map .liqs


getPoolNatives : Pool -> User -> Web (Maybe Natives)
getPoolNatives pool (User { natives }) =
    natives
        |> Remote.map (Dict.get pool)
