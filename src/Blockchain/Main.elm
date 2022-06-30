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
    , updateApproveAndBorrow
    , updateApproveAndLend
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
import Blockchain.User.Txns.TxnWrite exposing (TxnWrite)
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
import Data.Hash exposing (Hash)
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
    | OpenConfirm Int TxnWrite
    | SubmitTxn Int Hash
    | RejectTxn Int
    | ConfirmedTxn Hash


init :
    Chains
    -> String
    -> User.Flag
    -> Maybe ( Blockchain, Cmd Msg )
init chains endPoint flag =
    chains
        |> Chains.getGivenChainId flag.chainId
        |> Maybe.map
            (\chain ->
                User.init chains endPoint chain flag
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


update : { model | chains : Chains, endPoint : String } -> Msg -> Blockchain -> ( Blockchain, Cmd Msg, Maybe Effect )
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

        User.OpenConfirm id txnWrite ->
            OpenConfirm id txnWrite

        User.SubmitTxn id hash ->
            SubmitTxn id hash

        User.RejectTxn id ->
            RejectTxn id

        User.ConfirmedTxn hash ->
            ConfirmedTxn hash


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


updateApprove : ERC20 -> Blockchain -> ( Blockchain, Cmd Msg, Maybe Effect )
updateApprove erc20 (Blockchain ({ chain } as blockchain)) =
    blockchain.user
        |> Maybe.map
            (\user ->
                user
                    |> User.updateApprove chain erc20
                    |> (\( updated, cmd, effect ) ->
                            ( { blockchain | user = Just updated }
                                |> Blockchain
                            , cmd |> Cmd.map UserMsg
                            , effect |> userEffect |> Just
                            )
                       )
            )
        |> Maybe.withDefault
            ( blockchain |> Blockchain
            , Cmd.none
            , Nothing
            )


updateLend :
    { model | time : Posix, deadline : Deadline }
    -> WriteLend
    -> Blockchain
    -> ( Blockchain, Cmd Msg, Maybe Effect )
updateLend model writeLend (Blockchain ({ chain } as blockchain)) =
    blockchain.user
        |> Maybe.map
            (\user ->
                user
                    |> User.updateLend model chain writeLend
                    |> (\( updated, cmd, effect ) ->
                            ( { blockchain | user = Just updated }
                                |> Blockchain
                            , cmd |> Cmd.map UserMsg
                            , effect |> userEffect |> Just
                            )
                       )
            )
        |> Maybe.withDefault
            ( blockchain |> Blockchain
            , Cmd.none
            , Nothing
            )


updateApproveAndLend :
    { model | time : Posix, deadline : Deadline }
    -> WriteLend
    -> Blockchain
    -> ( Blockchain, Cmd Msg, Maybe Effect )
updateApproveAndLend model writeLend (Blockchain ({ chain } as blockchain)) =
    blockchain.user
        |> Maybe.map
            (\user ->
                user
                    |> User.updateApproveAndLend model chain writeLend
                    |> (\( updated, cmd, effect ) ->
                            ( { blockchain | user = Just updated }
                                |> Blockchain
                            , cmd |> Cmd.map UserMsg
                            , effect |> userEffect |> Just
                            )
                       )
            )
        |> Maybe.withDefault
            ( blockchain |> Blockchain
            , Cmd.none
            , Nothing
            )


updateBorrow :
    { model | time : Posix, deadline : Deadline }
    -> WriteBorrow
    -> Blockchain
    -> ( Blockchain, Cmd Msg, Maybe Effect )
updateBorrow model writeBorrow (Blockchain ({ chain } as blockchain)) =
    blockchain.user
        |> Maybe.map
            (\user ->
                user
                    |> User.updateBorrow model chain writeBorrow
                    |> (\( updated, cmd, effect ) ->
                            ( { blockchain | user = Just updated }
                                |> Blockchain
                            , cmd |> Cmd.map UserMsg
                            , effect |> userEffect |> Just
                            )
                       )
            )
        |> Maybe.withDefault
            ( blockchain |> Blockchain
            , Cmd.none
            , Nothing
            )


updateApproveAndBorrow :
    { model | time : Posix, deadline : Deadline }
    -> WriteBorrow
    -> Blockchain
    -> ( Blockchain, Cmd Msg, Maybe Effect )
updateApproveAndBorrow model writeBorrow (Blockchain ({ chain } as blockchain)) =
    blockchain.user
        |> Maybe.map
            (\user ->
                user
                    |> User.updateApproveAndBorrow model chain writeBorrow
                    |> (\( updated, cmd, effect ) ->
                            ( { blockchain | user = Just updated }
                                |> Blockchain
                            , cmd |> Cmd.map UserMsg
                            , effect |> userEffect |> Just
                            )
                       )
            )
        |> Maybe.withDefault
            ( blockchain |> Blockchain
            , Cmd.none
            , Nothing
            )


updateLiquidity :
    { model | time : Posix, deadline : Deadline }
    -> WriteLiquidity
    -> Blockchain
    -> ( Blockchain, Cmd Msg, Maybe Effect )
updateLiquidity model writeLiquidity (Blockchain ({ chain } as blockchain)) =
    blockchain.user
        |> Maybe.map
            (\user ->
                user
                    |> User.updateLiquidity model chain writeLiquidity
                    |> (\( updated, cmd, effect ) ->
                            ( { blockchain | user = Just updated }
                                |> Blockchain
                            , cmd |> Cmd.map UserMsg
                            , effect |> userEffect |> Just
                            )
                       )
            )
        |> Maybe.withDefault
            ( blockchain |> Blockchain
            , Cmd.none
            , Nothing
            )


updateCreate :
    { model | time : Posix, deadline : Deadline }
    -> WriteCreate
    -> Blockchain
    -> ( Blockchain, Cmd Msg, Maybe Effect )
updateCreate model writeCreate (Blockchain ({ chain } as blockchain)) =
    blockchain.user
        |> Maybe.map
            (\user ->
                user
                    |> User.updateCreate model chain writeCreate
                    |> (\( updated, cmd, effect ) ->
                            ( { blockchain | user = Just updated }
                                |> Blockchain
                            , cmd |> Cmd.map UserMsg
                            , effect |> userEffect |> Just
                            )
                       )
            )
        |> Maybe.withDefault
            ( blockchain |> Blockchain
            , Cmd.none
            , Nothing
            )


updateWithdraw :
    WriteWithdraw
    -> Blockchain
    -> ( Blockchain, Cmd Msg, Maybe Effect )
updateWithdraw writeWithdraw (Blockchain ({ chain } as blockchain)) =
    blockchain.user
        |> Maybe.map
            (\user ->
                user
                    |> User.updateWithdraw chain writeWithdraw
                    |> (\( updated, cmd, effect ) ->
                            ( { blockchain | user = Just updated }
                                |> Blockchain
                            , cmd |> Cmd.map UserMsg
                            , effect |> userEffect |> Just
                            )
                       )
            )
        |> Maybe.withDefault
            ( blockchain |> Blockchain
            , Cmd.none
            , Nothing
            )


updatePay :
    { model | time : Posix, deadline : Deadline }
    -> WritePay
    -> Blockchain
    -> ( Blockchain, Cmd Msg, Maybe Effect )
updatePay model writePay (Blockchain ({ chain } as blockchain)) =
    blockchain.user
        |> Maybe.map
            (\user ->
                user
                    |> User.updatePay model chain writePay
                    |> (\( updated, cmd, effect ) ->
                            ( { blockchain | user = Just updated }
                                |> Blockchain
                            , cmd |> Cmd.map UserMsg
                            , effect |> userEffect |> Just
                            )
                       )
            )
        |> Maybe.withDefault
            ( blockchain |> Blockchain
            , Cmd.none
            , Nothing
            )


updateBurn :
    WriteBurn
    -> Blockchain
    -> ( Blockchain, Cmd Msg, Maybe Effect )
updateBurn writeBurn (Blockchain ({ chain } as blockchain)) =
    blockchain.user
        |> Maybe.map
            (\user ->
                user
                    |> User.updateBurn chain writeBurn
                    |> (\( updated, cmd, effect ) ->
                            ( { blockchain | user = Just updated }
                                |> Blockchain
                            , cmd |> Cmd.map UserMsg
                            , effect |> userEffect |> Just
                            )
                       )
            )
        |> Maybe.withDefault
            ( blockchain |> Blockchain
            , Cmd.none
            , Nothing
            )


decoder : Chains -> Decoder (Maybe Chain)
decoder chains =
    Chains.decoderChain chains
        |> Decode.field "chainId"
        |> Decode.nullable


receiveUserInit :
    { model | key : Key, url : Url, chains : Chains, endPoint : String }
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
    { model | key : Key, url : Url, chains : Chains, endPoint : String }
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
