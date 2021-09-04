module Modals.Withdraw.Main exposing
    ( Modal
    , Msg
    , fromFragment
    , getPool
    , same
    , update
    )

import Data.Maturity as Maturity
import Data.Pool exposing (Pool)
import Data.Pools as Pools exposing (Pools)
import Data.Tokens exposing (Tokens)
import Time exposing (Posix)


type Modal
    = Modal Pool


init : Pool -> Modal
init pool =
    Modal pool


fromFragment :
    { model | time : Posix, tokens : Tokens, pools : Pools }
    -> String
    -> Maybe Modal
fromFragment { time, tokens, pools } string =
    string
        |> Pools.fromPoolFragment tokens pools
        |> Maybe.andThen
            (\({ maturity } as pool) ->
                if maturity |> Maturity.isActive time |> not then
                    Just pool

                else
                    Nothing
            )
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
