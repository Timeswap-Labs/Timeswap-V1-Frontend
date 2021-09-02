module Modals.Withdraw.Main exposing
    ( Modal
    , Msg
    , fromFragment
    , getPool
    , same
    , update
    )

import Data.Pool exposing (Pool)
import Data.Pools as Pools exposing (Pools)
import Data.Tokens exposing (Tokens)


type Modal
    = Modal Pool


init : Pool -> Modal
init pool =
    Modal pool


fromFragment : Tokens -> Pools -> String -> Maybe Modal
fromFragment tokens pools string =
    string
        |> Pools.fromPoolFragment tokens pools
        |> Maybe.map init


same : Modal -> Modal -> Bool
same (Modal pool1) (Modal pool2) =
    pool1 == pool2


getPool : Modal -> Pool
getPool (Modal pool) =
    pool


type Msg
    = Msg


update : Msg -> Modal -> ( Modal, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )
