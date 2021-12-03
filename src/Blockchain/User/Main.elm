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
        , balances : Web Balances
        , allowances : Web Allowances
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


init : Flag -> Maybe ( User, Cmd Msg )
init flag =
    case
        ( flag.wallet |> Wallet.init
        , flag.address |> Address.fromString
        )
    of
        ( Just wallet, Just address ) ->
            ( { wallet = wallet
              , address = address
              , name = Nothing
              , balances = Loading
              , allowances = Loading
              }
                |> User
            , Debug.todo "cmd"
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


decoder : Decoder (Maybe User)
decoder =
    Decode.succeed
        (\wallet address ->
            { wallet = wallet
            , address = address
            , name = Nothing
            , balances = Loading
            , allowances = Loading
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


receiveUserInit : Value -> Maybe ( User, Cmd Msg )
receiveUserInit value =
    case value |> Decode.decodeValue decoder of
        Ok (Just decodedUser) ->
            ( decodedUser
            , Debug.todo "cmd"
            )
                |> Just

        _ ->
            Nothing


receiveUser : Value -> User -> Maybe ( User, Cmd Msg )
receiveUser value user =
    case value |> Decode.decodeValue decoder of
        Ok (Just decodedUser) ->
            if user == decodedUser then
                ( user
                , Cmd.none
                )
                    |> Just

            else
                ( decodedUser
                , Debug.todo "cmd"
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


getBalance : Token -> User -> Maybe Uint
getBalance token (User { balances }) =
    balances
        |> Remote.map
            (\dict ->
                dict
                    |> Dict.get token
            )
        |> Remote.withDefault Nothing


hasEnoughBalance : Token -> Uint -> User -> Bool
hasEnoughBalance token amount (User { balances }) =
    balances
        |> Remote.map (Balances.hasEnough token amount)
        |> Remote.withDefault False


hasEnoughAllowance : ERC20 -> Uint -> User -> Bool
hasEnoughAllowance erc20 amount (User { allowances }) =
    allowances
        |> Remote.map (Allowances.hasEnough erc20 amount)
        |> Remote.withDefault False
