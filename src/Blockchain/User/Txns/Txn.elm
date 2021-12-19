module Blockchain.User.Txns.Txn exposing
    ( Flag
    , State(..)
    , Txn
    , decoderConfirmed
    , encodeConfirmed
    , initConfirmed
    )

import Blockchain.User.Txns.TxnWrite as TxnWrite exposing (TxnWrite)
import Data.Hash as Hash exposing (Hash)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)


type alias Txn =
    { id : Int
    , write : TxnWrite
    , state : State
    }


type State
    = Pending
    | Failed
    | Success


type alias Flag =
    { id : Int
    , hash : String
    , write : Value
    , state : String
    }


initConfirmed : Flag -> Maybe ( Hash, Txn )
initConfirmed flag =
    case
        ( flag.hash |> Hash.fromString
        , flag.write |> Decode.decodeValue TxnWrite.decoder
        , flag.state |> initState
        )
    of
        ( Just hash, Ok write, Just state ) ->
            ( hash
            , { id = flag.id
              , write = write
              , state = state
              }
            )
                |> Just

        _ ->
            Nothing


decoderConfirmed : Decoder ( Hash, Txn )
decoderConfirmed =
    Decode.succeed
        (\id hash write state ->
            ( hash
            , { id = id
              , write = write
              , state = state
              }
            )
        )
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "hash" Hash.decoder
        |> Pipeline.required "write" TxnWrite.decoder
        |> Pipeline.required "state" decoderState


initState : String -> Maybe State
initState string =
    case string of
        "pending" ->
            Just Pending

        "failed" ->
            Just Failed

        "success" ->
            Just Success

        _ ->
            Nothing


decoderState : Decoder State
decoderState =
    Decode.string
        |> Decode.andThen
            (\string ->
                string
                    |> initState
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault (Decode.fail "Not a state")
            )


encodeConfirmed : ( Hash, Txn ) -> Value
encodeConfirmed ( hash, { id, write, state } ) =
    [ ( "id", id |> Encode.int )
    , ( "hash", hash |> Hash.encode )
    , ( "write", write |> TxnWrite.encode )
    , ( "state", state |> encodeState )
    ]
        |> Encode.object


encodeState : State -> Value
encodeState state =
    (case state of
        Pending ->
            "pending"

        Failed ->
            "failed"

        Success ->
            "success"
    )
        |> Encode.string
