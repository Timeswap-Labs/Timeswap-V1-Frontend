module Modals.Pay.Main exposing
    ( Flags
    , Modal
    , Msg
    , fromFragment
    , getFlags
    , same
    , update
    )

import Data.Pool exposing (Pool)
import Data.Pools as Pools exposing (Pools)
import Data.TokenId as TokenId exposing (TokenId)
import Data.Tokens exposing (Tokens)
import Sort.Set exposing (Set)


type Modal
    = Modal
        { pool : Pool
        , tokenIds : Set TokenId
        }


type alias Flags =
    { pool : Pool
    , tokenIds : Set TokenId
    }


init : Flags -> Modal
init { pool, tokenIds } =
    { pool = pool
    , tokenIds = tokenIds
    }
        |> Modal


fromFragment : Tokens -> Pools -> String -> Maybe Modal
fromFragment tokens pools string =
    string
        |> String.split "&"
        |> (\list ->
                case list of
                    asset :: collateral :: maturity :: tokenIds :: _ ->
                        Maybe.map2 Flags
                            ([ asset
                             , collateral
                             , maturity
                             ]
                                |> String.join "&"
                                |> Pools.fromPoolFragment tokens pools
                            )
                            (tokenIds |> TokenId.fromFragment)
                            |> Maybe.map init

                    _ ->
                        Nothing
           )


same : Modal -> Modal -> Bool
same (Modal modal1) (Modal modal2) =
    modal1.pool == modal2.pool && modal1.tokenIds == modal2.tokenIds


getFlags : Modal -> Flags
getFlags (Modal { pool, tokenIds }) =
    { pool = pool
    , tokenIds = tokenIds
    }


type Msg
    = Msg


update : Msg -> Modal -> ( Modal, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )
