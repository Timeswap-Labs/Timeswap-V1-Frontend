module Blockchain.Main exposing
    ( Blockchain(..)
    , init
    , toChain
    , toUser
    , updateUser
    )

import Blockchain.User.Main as User exposing (User)
import Browser.Navigation as Navigation exposing (Key)
import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Url exposing (Url)


type Blockchain
    = Supported
        { chain : Chain
        , user : Maybe User
        }
    | NotSupported User.NotSupported


init : Chains -> Maybe User.Flag -> Blockchain
init chains flag =
    flag
        |> Maybe.andThen (User.init chains)
        |> Maybe.map
            (\result ->
                case result of
                    Ok ( chain, user ) ->
                        { chain = chain
                        , user = Just user
                        }
                            |> Supported

                    Err user ->
                        user
                            |> NotSupported
            )
        |> Maybe.withDefault
            ({ chain = chains |> Chains.head
             , user = Nothing
             }
                |> Supported
            )


updateUser :
    { model | key : Key, url : Url, chains : Chains }
    -> Value
    -> Blockchain
    -> ( Blockchain, Cmd msg )
updateUser { key, url, chains } value blockchain =
    case
        value
            |> Decode.decodeValue
                (User.decoder chains)
    of
        Ok (Just (Ok ( chain, user ))) ->
            case blockchain of
                Supported block ->
                    if
                        (chain == block.chain)
                            && (Just user == block.user)
                    then
                        ( blockchain
                        , Cmd.none
                        )

                    else if chain == block.chain then
                        ( { block | user = Just user }
                            |> Supported
                        , Cmd.none
                        )

                    else
                        ( { chain = chain
                          , user = Just user
                          }
                            |> Supported
                        , url
                            |> Url.toString
                            |> Navigation.pushUrl key
                        )

                _ ->
                    ( { chain = chain
                      , user = Just user
                      }
                        |> Supported
                    , url
                        |> Url.toString
                        |> Navigation.pushUrl key
                    )

        Ok (Just (Err user)) ->
            ( user |> NotSupported
            , url
                |> Url.toString
                |> Navigation.pushUrl key
            )

        Ok Nothing ->
            case blockchain of
                Supported block ->
                    ( { block | user = Nothing }
                        |> Supported
                    , Cmd.none
                    )

                NotSupported _ ->
                    ( { chain = chains |> Chains.head
                      , user = Nothing
                      }
                        |> Supported
                    , url
                        |> Url.toString
                        |> Navigation.pushUrl key
                    )

        Err _ ->
            ( blockchain
            , Cmd.none
            )


toUser : Blockchain -> Maybe (Result User.NotSupported User)
toUser blockchain =
    case blockchain of
        Supported { user } ->
            user
                |> Maybe.map Ok

        NotSupported user ->
            user
                |> Err
                |> Just


toChain : Blockchain -> Maybe Chain
toChain blockchain =
    case blockchain of
        Supported { chain } ->
            Just chain

        _ ->
            Nothing
