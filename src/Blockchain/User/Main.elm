module Blockchain.User.Main exposing
    ( Flag
    , Msg
    , NotSupported
    , User
    , init
    , initNotSupported
    , receiveNotSupported
    , receiveUser
    , receiveUserInit
    , update
    )

import Blockchain.User.Allowances exposing (Allowances)
import Blockchain.User.Balances exposing (Balances)
import Data.Address as Address exposing (Address)
import Data.Remote exposing (Remote(..))
import Data.Wallet as Wallet exposing (Wallet)
import Data.Web exposing (Web)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode exposing (Value)


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
