module Blockchain.Main exposing
    ( Blockchain
    , Effect(..)
    , Msg
    , init
    , initDefault
    , initGivenChain
    , receiveUser
    , receiveUserInit
    , subscriptions
    , toChain
    , toUser
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

import Blockchain.User.Main as User exposing (User)
import Blockchain.User.WriteBorrow exposing (WriteBorrow)
import Blockchain.User.WriteBurn exposing (WriteBurn)
import Blockchain.User.WriteCreate exposing (WriteCreate)
import Blockchain.User.WriteLend exposing (WriteLend)
import Blockchain.User.WriteLiquidity exposing (WriteLiquidity)
import Blockchain.User.WritePay exposing (WritePay)
import Blockchain.User.WriteWithdraw exposing (WriteWithdraw)
import Browser.Navigation as Navigation exposing (Key)
import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.Deadline exposing (Deadline)
import Data.ERC20 exposing (ERC20)
import Data.ERC20s exposing (ERC20s)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Time exposing (Posix)
import Url exposing (Url)


type Blockchain
    = Blockchain
        { chain : Chain
        , user : Maybe User
        }


type Msg
    = UserMsg User.Msg


type Effect
    = AddERC20s ERC20s


init :
    Chains
    -> User.Flag
    -> Maybe ( Blockchain, Cmd Msg )
init chains flag =
    chains
        |> Chains.getGivenChainId flag.chainId
        |> Maybe.map
            (\chain ->
                User.init chains chain flag
                    |> Maybe.map
                        (\( user, cmd ) ->
                            ( { chain = chain
                              , user = Just user
                              }
                                |> Blockchain
                            , [ cmd |> Cmd.map UserMsg
                              , Cmd.none
                              ]
                                |> Cmd.batch
                            )
                        )
                    |> Maybe.withDefault
                        ( { chain = chain
                          , user = Nothing
                          }
                            |> Blockchain
                        , Cmd.none
                          -- |> Debug.log "cmd"
                        )
            )


initDefault : Chains -> ( Blockchain, Cmd Msg )
initDefault chains =
    chains
        |> Chains.head
        |> initGivenChain


initGivenChain : Chain -> ( Blockchain, Cmd Msg )
initGivenChain chain =
    ( { chain = chain
      , user = Nothing
      }
        |> Blockchain
    , Cmd.none
      -- |> Debug.log "cmd"
    )


update : { model | chains : Chains } -> Msg -> Blockchain -> ( Blockchain, Cmd Msg, Maybe Effect )
update model msg (Blockchain blockchain) =
    case msg of
        UserMsg userMsg ->
            blockchain.user
                |> Maybe.map (User.update model blockchain.chain userMsg)
                |> Maybe.map
                    (\( user, cmd, maybeEffect ) ->
                        ( { blockchain
                            | user = Just user
                          }
                            |> Blockchain
                        , cmd |> Cmd.map UserMsg
                        , maybeEffect |> Maybe.map userEffect
                        )
                    )
                |> Maybe.withDefault
                    ( blockchain |> Blockchain
                    , Cmd.none
                    , Nothing
                    )


userEffect : User.Effect -> Effect
userEffect effect =
    case effect of
        User.AddERC20s erc20s ->
            AddERC20s erc20s


updateClearTxns : Blockchain -> ( Blockchain, Cmd Msg )
updateClearTxns (Blockchain ({ chain } as blockchain)) =
    blockchain.user
        |> Maybe.map
            (\user ->
                user
                    |> User.updateClearTxns chain
                    |> Tuple.mapBoth
                        (\updated ->
                            { blockchain | user = Just updated }
                                |> Blockchain
                        )
                        (Cmd.map UserMsg)
            )
        |> Maybe.withDefault
            ( blockchain |> Blockchain
            , Cmd.none
            )


updateAddERC20 : ERC20 -> Blockchain -> ( Blockchain, Cmd Msg )
updateAddERC20 erc20 (Blockchain ({ chain } as blockchain)) =
    blockchain.user
        |> Maybe.map
            (\user ->
                user
                    |> User.updateAddERC20 chain erc20
                    |> Tuple.mapBoth
                        (\updated ->
                            { blockchain | user = Just updated }
                                |> Blockchain
                        )
                        (Cmd.map UserMsg)
            )
        |> Maybe.withDefault
            ( blockchain |> Blockchain
            , Cmd.none
            )


updateApprove : ERC20 -> Blockchain -> ( Blockchain, Cmd Msg )
updateApprove erc20 (Blockchain ({ chain } as blockchain)) =
    blockchain.user
        |> Maybe.map
            (\user ->
                user
                    |> User.updateApprove chain erc20
                    |> Tuple.mapBoth
                        (\updated ->
                            { blockchain | user = Just updated }
                                |> Blockchain
                        )
                        (Cmd.map UserMsg)
            )
        |> Maybe.withDefault
            ( blockchain |> Blockchain
            , Cmd.none
            )


updateLend :
    { model | time : Posix, deadline : Deadline }
    -> WriteLend
    -> Blockchain
    -> ( Blockchain, Cmd Msg )
updateLend model writeLend (Blockchain ({ chain } as blockchain)) =
    blockchain.user
        |> Maybe.map
            (\user ->
                user
                    |> User.updateLend model chain writeLend
                    |> Tuple.mapBoth
                        (\updated ->
                            { blockchain | user = Just updated }
                                |> Blockchain
                        )
                        (Cmd.map UserMsg)
            )
        |> Maybe.withDefault
            ( blockchain |> Blockchain
            , Cmd.none
            )


updateBorrow :
    { model | time : Posix, deadline : Deadline }
    -> WriteBorrow
    -> Blockchain
    -> ( Blockchain, Cmd Msg )
updateBorrow model writeBorrow (Blockchain ({ chain } as blockchain)) =
    blockchain.user
        |> Maybe.map
            (\user ->
                user
                    |> User.updateBorrow model chain writeBorrow
                    |> Tuple.mapBoth
                        (\updated ->
                            { blockchain | user = Just updated }
                                |> Blockchain
                        )
                        (Cmd.map UserMsg)
            )
        |> Maybe.withDefault
            ( blockchain |> Blockchain
            , Cmd.none
            )


updateLiquidity :
    { model | time : Posix, deadline : Deadline }
    -> WriteLiquidity
    -> Blockchain
    -> ( Blockchain, Cmd Msg )
updateLiquidity model writeLiquidity (Blockchain ({ chain } as blockchain)) =
    blockchain.user
        |> Maybe.map
            (\user ->
                user
                    |> User.updateLiquidity model chain writeLiquidity
                    |> Tuple.mapBoth
                        (\updated ->
                            { blockchain | user = Just updated }
                                |> Blockchain
                        )
                        (Cmd.map UserMsg)
            )
        |> Maybe.withDefault
            ( blockchain |> Blockchain
            , Cmd.none
            )


updateCreate :
    { model | time : Posix, deadline : Deadline }
    -> WriteCreate
    -> Blockchain
    -> ( Blockchain, Cmd Msg )
updateCreate model writeCreate (Blockchain ({ chain } as blockchain)) =
    blockchain.user
        |> Maybe.map
            (\user ->
                user
                    |> User.updateCreate model chain writeCreate
                    |> Tuple.mapBoth
                        (\updated ->
                            { blockchain | user = Just updated }
                                |> Blockchain
                        )
                        (Cmd.map UserMsg)
            )
        |> Maybe.withDefault
            ( blockchain |> Blockchain
            , Cmd.none
            )


updateWithdraw :
    WriteWithdraw
    -> Blockchain
    -> ( Blockchain, Cmd Msg )
updateWithdraw writeWithdraw (Blockchain ({ chain } as blockchain)) =
    blockchain.user
        |> Maybe.map
            (\user ->
                user
                    |> User.updateWithdraw chain writeWithdraw
                    |> Tuple.mapBoth
                        (\updated ->
                            { blockchain | user = Just updated }
                                |> Blockchain
                        )
                        (Cmd.map UserMsg)
            )
        |> Maybe.withDefault
            ( blockchain |> Blockchain
            , Cmd.none
            )


updatePay :
    { model | time : Posix, deadline : Deadline }
    -> WritePay
    -> Blockchain
    -> ( Blockchain, Cmd Msg )
updatePay model writePay (Blockchain ({ chain } as blockchain)) =
    blockchain.user
        |> Maybe.map
            (\user ->
                user
                    |> User.updatePay model chain writePay
                    |> Tuple.mapBoth
                        (\updated ->
                            { blockchain | user = Just updated }
                                |> Blockchain
                        )
                        (Cmd.map UserMsg)
            )
        |> Maybe.withDefault
            ( blockchain |> Blockchain
            , Cmd.none
            )


updateBurn :
    WriteBurn
    -> Blockchain
    -> ( Blockchain, Cmd Msg )
updateBurn writeBurn (Blockchain ({ chain } as blockchain)) =
    blockchain.user
        |> Maybe.map
            (\user ->
                user
                    |> User.updateBurn chain writeBurn
                    |> Tuple.mapBoth
                        (\updated ->
                            { blockchain | user = Just updated }
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
receiveUserInit ({ key, url, chains } as model) value =
    case
        value
            |> Decode.decodeValue
                (decoder chains)
    of
        Ok (Just chain) ->
            User.receiveUserInit model chain value
                |> Maybe.map
                    (\( user, cmd ) ->
                        ( { chain = chain
                          , user = Just user
                          }
                            |> Blockchain
                        , [ cmd |> Cmd.map UserMsg
                          , Cmd.none

                          -- |> Debug.log "cmd"
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
                     , [ Cmd.none

                       -- |> Debug.log "cmd"
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
            , [ Cmd.none

              -- |> Debug.log "cmd"
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
receiveUser ({ key, url, chains } as model) value (Blockchain blockchain) =
    case
        value
            |> Decode.decodeValue
                (decoder chains)
    of
        Ok (Just chain) ->
            if chain == blockchain.chain then
                blockchain.user
                    |> Maybe.map (User.receiveUser model chain value)
                    |> Maybe.withDefault
                        (User.receiveUserInit model chain value)
                    |> Maybe.map
                        (\( user, cmd ) ->
                            ( { blockchain | user = Just user }
                                |> Blockchain
                            , cmd |> Cmd.map UserMsg
                            )
                        )

            else
                User.receiveUserInit model chain value
                    |> Maybe.map
                        (\( user, cmd ) ->
                            ( { chain = chain
                              , user = Just user
                              }
                                |> Blockchain
                            , [ cmd |> Cmd.map UserMsg
                              , Cmd.none

                              -- |> Debug.log "cmd"
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
                         , [ Cmd.none

                           -- |> Debug.log "cmd"
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
                , [ Cmd.none

                  -- |> Debug.log "cmd"
                  , url
                        |> Url.toString
                        |> Navigation.pushUrl key
                  ]
                    |> Cmd.batch
                )
                    |> Just

        Err _ ->
            Nothing


subscriptions : Blockchain -> Sub Msg
subscriptions blockchain =
    blockchain
        |> toUser
        |> Maybe.map
            (\user ->
                user
                    |> User.subscriptions
                    |> Sub.map UserMsg
            )
        |> Maybe.withDefault Sub.none


toUser : Blockchain -> Maybe User
toUser (Blockchain { user }) =
    user


toChain : Blockchain -> Chain
toChain (Blockchain { chain }) =
    chain
