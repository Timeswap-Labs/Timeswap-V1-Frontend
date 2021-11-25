module Blockchain.Main exposing
    ( Blockchain
    , Msg
    , init
    , initDefault
    , receiveUser
    , receiveUserInit
    , toChain
    , toUser
    , update
    )

import Blockchain.User.Main as User exposing (User)
import Browser.Navigation as Navigation exposing (Key)
import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.Wallet as Wallet
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Url exposing (Url)


type Blockchain
    = Blockchain
        { chain : Chain
        , user : Maybe User
        }


type Msg
    = UserMsg User.Msg


init :
    Chains
    -> User.Flag
    -> Maybe ( Blockchain, Cmd Msg )
init chains flag =
    chains
        |> Chains.getGivenChainId flag.chainId
        |> Maybe.map
            (\chain ->
                User.init flag
                    |> Maybe.map
                        (\( user, cmd ) ->
                            ( { chain = chain
                              , user = Just user
                              }
                                |> Blockchain
                            , [ cmd |> Cmd.map UserMsg
                              , Cmd.none |> Debug.log "cmd"
                              ]
                                |> Cmd.batch
                            )
                        )
                    |> Maybe.withDefault
                        ( { chain = chain
                          , user = Nothing
                          }
                            |> Blockchain
                        , Cmd.none |> Debug.log "cmd"
                        )
            )


initDefault : Chains -> ( Blockchain, Cmd Msg )
initDefault chains =
    chains
        |> Chains.head
        |> (\chain ->
                ( { chain = chain
                  , user = Nothing
                  }
                    |> Blockchain
                , Cmd.none |> Debug.log "cmd"
                )
           )


update : Msg -> Blockchain -> ( Blockchain, Cmd Msg )
update msg (Blockchain blockchain) =
    case msg of
        UserMsg userMsg ->
            blockchain.user
                |> Maybe.map (User.update userMsg)
                |> Maybe.map
                    (Tuple.mapBoth
                        (\user ->
                            { blockchain
                                | user = Just user
                            }
                                |> Blockchain
                        )
                        (Cmd.map UserMsg)
                    )
                |> Maybe.withDefault
                    ( blockchain |> Blockchain
                    , Cmd.none
                    )


decoder : Chains -> Decoder (Maybe Chain)
decoder chains =
    Chains.decoderChain chains
        |> Decode.field "chainId"
        |> Decode.nullable


receiveUserInit :
    { model | key : Key, url : Url, chains : Chains }
    -> Value
    -> Maybe ( Blockchain, Cmd Msg )
receiveUserInit { key, url, chains } value =
    case
        value
            |> Decode.decodeValue
                (decoder chains)
    of
        Ok (Just chain) ->
            User.receiveUserInit value
                |> Maybe.map
                    (\( user, cmd ) ->
                        ( { chain = chain
                          , user = Just user
                          }
                            |> Blockchain
                        , [ cmd |> Cmd.map UserMsg
                          , Cmd.none |> Debug.log "cmd"
                          , url
                                |> Url.toString
                                |> Navigation.pushUrl key
                          ]
                            |> Cmd.batch
                        )
                            |> Just
                    )
                |> Maybe.withDefault
                    (( { chain = chain
                       , user = Nothing
                       }
                        |> Blockchain
                     , [ Cmd.none |> Debug.log "cmd"
                       , url
                            |> Url.toString
                            |> Navigation.pushUrl key
                       ]
                        |> Cmd.batch
                     )
                        |> Just
                    )

        Ok Nothing ->
            ( { chain = chains |> Chains.head
              , user = Nothing
              }
                |> Blockchain
            , [ Cmd.none |> Debug.log "cmd"
              , url
                    |> Url.toString
                    |> Navigation.pushUrl key
              ]
                |> Cmd.batch
            )
                |> Just

        Err _ ->
            Nothing


receiveUser :
    { model | key : Key, url : Url, chains : Chains }
    -> Value
    -> Blockchain
    -> Maybe ( Blockchain, Cmd Msg )
receiveUser { key, url, chains } value (Blockchain blockchain) =
    case
        value
            |> Decode.decodeValue
                (decoder chains)
    of
        Ok (Just chain) ->
            if chain == blockchain.chain then
                blockchain.user
                    |> Maybe.map (User.receiveUser value)
                    |> Maybe.withDefault
                        (User.receiveUserInit value)
                    |> Maybe.map
                        (\( user, cmd ) ->
                            ( { blockchain | user = Just user }
                                |> Blockchain
                            , cmd |> Cmd.map UserMsg
                            )
                        )

            else
                User.receiveUserInit value
                    |> Maybe.map
                        (\( user, cmd ) ->
                            ( { chain = chain
                              , user = Just user
                              }
                                |> Blockchain
                            , [ cmd |> Cmd.map UserMsg
                              , Cmd.none |> Debug.log "cmd"
                              , url
                                    |> Url.toString
                                    |> Navigation.pushUrl key
                              ]
                                |> Cmd.batch
                            )
                                |> Just
                        )
                    |> Maybe.withDefault
                        (( { chain = chain
                           , user = Nothing
                           }
                            |> Blockchain
                         , [ Cmd.none |> Debug.log "cmd"
                           , url
                                |> Url.toString
                                |> Navigation.pushUrl key
                           ]
                            |> Cmd.batch
                         )
                            |> Just
                        )

        Ok Nothing ->
            if (chains |> Chains.head) == blockchain.chain then
                ( { blockchain | user = Nothing }
                    |> Blockchain
                , Cmd.none
                )
                    |> Just

            else
                ( { chain = chains |> Chains.head
                  , user = Nothing
                  }
                    |> Blockchain
                , [ Cmd.none |> Debug.log "cmd"
                  , url
                        |> Url.toString
                        |> Navigation.pushUrl key
                  ]
                    |> Cmd.batch
                )
                    |> Just

        Err _ ->
            Nothing


toUser : Blockchain -> Maybe User
toUser (Blockchain { user }) =
    user


toChain : Blockchain -> Chain
toChain (Blockchain { chain }) =
    chain
