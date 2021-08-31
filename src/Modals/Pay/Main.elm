module Modals.Pay.Main exposing
    ( Flags
    , Modal
    , Msg
    , fromFragment
    , getPool
    , init
    , same
    , update
    )

import Data.Chain exposing (Chain)
import Data.Maturity as Maturity exposing (Maturity)
import Data.Pair as Pair exposing (Pair)
import Data.TokenId as TokenId exposing (TokenId)
import Sort.Set exposing (Set)


type Modal
    = Modal
        { pair : Pair
        , maturity : Maturity
        , tokenIds : Set TokenId
        }


type alias Flags =
    { pair : Pair
    , maturity : Maturity
    , tokenIds : Set TokenId
    }


init : Flags -> Modal
init { pair, maturity, tokenIds } =
    { pair = pair
    , maturity = maturity
    , tokenIds = tokenIds
    }
        |> Modal


fromFragment : Chain -> String -> Maybe Modal
fromFragment chain string =
    string
        |> String.split "&"
        |> (\list ->
                case list of
                    asset :: collateral :: maturity :: tokenIds :: _ ->
                        Maybe.map3 Flags
                            (Pair.fromFragment chain asset collateral)
                            (maturity |> Maturity.fromFragment)
                            (tokenIds |> TokenId.fromFragment)
                            |> Maybe.map init

                    _ ->
                        Nothing
           )


same : Modal -> Modal -> Bool
same (Modal modal1) (Modal modal2) =
    modal1.pair == modal2.pair && modal1.maturity == modal2.maturity && modal1.tokenIds == modal2.tokenIds


getPool : Modal -> Flags
getPool (Modal { pair, maturity, tokenIds }) =
    { pair = pair
    , maturity = maturity
    , tokenIds = tokenIds
    }


type Msg
    = Msg


update : Msg -> Modal -> ( Modal, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )
