module Modals.Pay.Main exposing
    ( Flags
    , Modal
    , Msg
    , fromFragment
    , toFragment
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


toFragment : Modal -> String
toFragment (Modal { pair, maturity, tokenIds }) =
    [ pair |> Pair.toFragment
    , maturity |> Maturity.toFragment
    , tokenIds |> TokenId.toFragment
    ]
        |> String.join "&"


type Msg
    = Msg


update : Msg -> Modal -> ( Modal, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )
