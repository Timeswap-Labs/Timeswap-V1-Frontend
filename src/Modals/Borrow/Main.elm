module Modals.Borrow.Main exposing
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
    = Modal { pool : Pool }


init : Pool -> Modal
init pool =
    { pool = pool }
        |> Modal


fromFragment : Tokens -> Pools -> String -> Maybe Modal
fromFragment tokens pools string =
    string
        |> Pools.fromPoolFragment tokens pools
        |> Maybe.map init


same : Modal -> Modal -> Bool
same (Modal modal1) (Modal modal2) =
    modal1.pool == modal2.pool


getPool : Modal -> Pool
getPool (Modal { pool }) =
    pool


type Msg
    = Msg


update : Msg -> Modal -> ( Modal, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )
