module Blockchain.User.Main exposing
    ( Flag
    , Msg
    , NotSupported
    , User
    , getBalance
    , hasEnoughAllowance
    , hasEnoughBalance
    , init
    , initNotSupported
    , receiveNotSupported
    , receiveUser
    , receiveUserInit
    , toAddress
    , toAddressNotSupported
    , toName
    , toWallet
    , toWalletNotSupported
    , update
    )

import Blockchain.User.Allowances as Allowances exposing (Allowances)
import Blockchain.User.Balances as Balances exposing (Balances)
import Data.Address as Address exposing (Address)
import Data.Chain as Chain exposing (Chain)
import Data.Chains exposing (Chains)
import Data.ERC20 exposing (ERC20)
import Data.Remote as Remote exposing (Remote(..))
import Data.Token exposing (Token)
import Data.Uint exposing (Uint)
import Data.Wallet as Wallet exposing (Wallet)
import Data.Web exposing (Web)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode exposing (Value)
import Sort.Dict as Dict


type User
    = User
        { wallet : Wallet
        , address : Address
        , name : Maybe String
        , balances : Balances
        , allowances : Allowances
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
    }


type Msg
    = Msg


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
              }
                |> User
            , Cmd.none |> Debug.log "Later"
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


update : Msg -> User -> ( User, Cmd Msg )
update msg (User user) =
    ( User user
    , Cmd.none
    )


decoder :
    { model | chains : Chains }
    -> Chain
    -> Decoder (Maybe User)
decoder { chains } chain =
    Decode.succeed
        (\wallet address ->
            { wallet = wallet
            , address = address
            , name = Nothing
            , balances = Balances.init chains chain
            , allowances = Allowances.init chains chain
            }
                |> User
        )
        |> Pipeline.required "wallet" Wallet.decoder
        |> Pipeline.required "address" Address.decoder
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
            , Cmd.none |> Debug.log "Later"
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
                , Cmd.none |> Debug.log "Later"
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


hasEnoughBalance : Token -> Uint -> User -> Bool
hasEnoughBalance token amount (User { balances }) =
    balances
        |> Balances.hasEnough token amount


hasEnoughAllowance : ERC20 -> Uint -> User -> Bool
hasEnoughAllowance erc20 amount (User { allowances }) =
    allowances
        |> Allowances.hasEnough erc20 amount
