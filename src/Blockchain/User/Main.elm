port module Blockchain.User.Main exposing
    ( Flag
    , Msg
    , NotSupported
    , User
    , getAllowance
    , getBalance
    , getClaims
    , getDues
    , getLiqs
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
    , updateWithdraw
    )

import Blockchain.User.Allowances as Allowances exposing (Allowances)
import Blockchain.User.Balances as Balances exposing (Balances)
import Blockchain.User.Cache as Cache
import Blockchain.User.Claims exposing (Claims)
import Blockchain.User.Dues exposing (Dues)
import Blockchain.User.Liqs exposing (Liqs)
import Blockchain.User.Positions as Positions exposing (Positions)
import Blockchain.User.Txns.Main as Txns exposing (Txns)
import Blockchain.User.Txns.Txn as Txn exposing (Txn)
import Blockchain.User.Txns.TxnWrite as TxnWrite
import Blockchain.User.Write as Write
import Blockchain.User.WriteApprove as WriteApprove
import Blockchain.User.WriteBorrow as WriteBorrow exposing (WriteBorrow)
import Blockchain.User.WriteBurn as WriteBurn exposing (WriteBurn)
import Blockchain.User.WriteCreate as WriteCreate exposing (WriteCreate)
import Blockchain.User.WriteLend as WriteLend exposing (WriteLend)
import Blockchain.User.WriteLiquidity as WriteLiquidity exposing (WriteLiquidity)
import Blockchain.User.WriteWithdraw as WriteWithdraw exposing (WriteWithdraw)
import Data.Address as Address exposing (Address)
import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.Deadline exposing (Deadline)
import Data.ERC20 exposing (ERC20)
import Data.Hash exposing (Hash)
import Data.Remote as Remote exposing (Remote(..))
import Data.Token as Token exposing (Token)
import Data.Uint exposing (Uint)
import Data.Wallet as Wallet exposing (Wallet)
import Data.Web exposing (Web)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode exposing (Value)
import Sort.Dict as Dict
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
    = ReceiveBalances Value
    | ReceiveAllowances Value
    | ReceiveReceipt Value
    | BalancesTick Posix
    | AllowancesTick Posix


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
              , positions = Success Positions.dummy

              --   |> Debug.log "replace"
              , txns = flag.txns |> Txns.init
              }
                |> User
            , [ address
                    |> Balances.encode chains chain
                    |> balancesOf
              , address
                    |> Allowances.encode chains chain
                    |> allowancesOf
              ]
                |> Cmd.batch
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


update : Chain -> Msg -> User -> ( User, Cmd Msg )
update chain msg (User user) =
    case msg of
        ReceiveBalances value ->
            case value |> Decode.decodeValue Balances.decoder of
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
                            |> noCmd

                    else
                        user |> noCmd

                Err _ ->
                    user |> noCmd

        ReceiveAllowances value ->
            case value |> Decode.decodeValue Allowances.decoder of
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
                            |> noCmd

                    else
                        user |> noCmd

                Err _ ->
                    user |> noCmd

        ReceiveReceipt value ->
            case value |> Decode.decodeValue Write.decoder of
                Ok receipt ->
                    if
                        (receipt.chain == chain)
                            && (receipt.address == user.address)
                    then
                        user.txns
                            |> Txns.confirm receipt.id
                                receipt.hash
                                Txn.Pending
                            |> (\txns ->
                                    ( { user | txns = txns }
                                        |> User
                                    , txns
                                        |> Cache.encodeTxns chain user.address
                                        |> cacheTxns
                                    )
                               )

                    else
                        user |> noCmd

                Err _ ->
                    user |> noCmd

        BalancesTick posix ->
            ( { user | balances = user.balances |> Balances.update posix }
                |> User
            , Cmd.none
            )

        AllowancesTick posix ->
            ( { user | allowances = user.allowances |> Allowances.update posix }
                |> User
            , Cmd.none
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


updateApprove : Chain -> ERC20 -> User -> ( User, Cmd Msg )
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
                )
           )


updateLend :
    { model | time : Posix, deadline : Deadline }
    -> Chain
    -> WriteLend
    -> User
    -> ( User, Cmd Msg )
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
                )
           )


updateBorrow :
    { model | time : Posix, deadline : Deadline }
    -> Chain
    -> WriteBorrow
    -> User
    -> ( User, Cmd Msg )
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
                )
           )


updateLiquidity :
    { model | time : Posix, deadline : Deadline }
    -> Chain
    -> WriteLiquidity
    -> User
    -> ( User, Cmd Msg )
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
                )
           )


updateCreate :
    { model | time : Posix, deadline : Deadline }
    -> Chain
    -> WriteCreate
    -> User
    -> ( User, Cmd Msg )
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
                )
           )


updateWithdraw :
    Chain
    -> WriteWithdraw
    -> User
    -> ( User, Cmd Msg )
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
                )
           )


updateBurn :
    Chain
    -> WriteBurn
    -> User
    -> ( User, Cmd Msg )
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
                        |> withdraw
                  , txns
                        |> Cache.encodeTxns chain user.address
                        |> cacheTxns
                  ]
                    |> Cmd.batch
                )
           )


noCmd :
    { wallet : Wallet
    , address : Address
    , name : Maybe String
    , balances : Balances
    , allowances : Allowances
    , positions : Web Positions
    , txns : Txns
    }
    -> ( User, Cmd Msg )
noCmd user =
    ( user |> User
    , Cmd.none
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
            , positions = Success Positions.dummy

            -- |> Debug.log "replace"
            , txns = txns
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


receiveUserInit :
    { model | chains : Chains }
    -> Chain
    -> Value
    -> Maybe ( User, Cmd Msg )
receiveUserInit model chain value =
    case value |> Decode.decodeValue (decoder model chain) of
        Ok (Just decodedUser) ->
            ( decodedUser
            , Cmd.none
              -- |> Debug.log "query for name"
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
receiveUser model chain value user =
    case value |> Decode.decodeValue (decoder model chain) of
        Ok (Just decodedUser) ->
            if isSame user decodedUser then
                ( user
                , Cmd.none
                )
                    |> Just

            else
                ( decodedUser
                , Cmd.none
                  -- |> Debug.log "query for name"
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


port cacheTxns : Value -> Cmd msg


port balancesOf : Value -> Cmd msg


port allowancesOf : Value -> Cmd msg


port approve : Value -> Cmd msg


port lend : Value -> Cmd msg


port borrow : Value -> Cmd msg


port liquidity : Value -> Cmd msg


port create : Value -> Cmd msg


port withdraw : Value -> Cmd msg


port receiveBalances : (Value -> msg) -> Sub msg


port receiveAllowances : (Value -> msg) -> Sub msg


port receiveReceipt : (Value -> msg) -> Sub msg


subscriptions : User -> Sub Msg
subscriptions (User user) =
    [ receiveBalances ReceiveBalances
    , receiveAllowances ReceiveAllowances
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
        |> Remote.map .liquidities
