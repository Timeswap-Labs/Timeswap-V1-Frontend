module Modals.Withdraw.Main exposing
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


type Modal
    = Modal
        { pair : Pair
        , maturity : Maturity
        }


type alias Flags =
    { pair : Pair
    , maturity : Maturity
    }


init : Flags -> Modal
init { pair, maturity } =
    { pair = pair
    , maturity = maturity
    }
        |> Modal


fromFragment : Chain -> String -> Maybe Modal
fromFragment chain string =
    string
        |> String.split "&"
        |> (\list ->
                case list of
                    asset :: collateral :: maturity :: _ ->
                        Maybe.map2 Flags
                            (Pair.fromFragment chain asset collateral)
                            (maturity |> Maturity.fromFragment)
                            |> Maybe.map init

                    _ ->
                        Nothing
           )


same : Modal -> Modal -> Bool
same (Modal modal1) (Modal modal2) =
    modal1.pair == modal2.pair && modal1.maturity == modal2.maturity


getPool : Modal -> Flags
getPool (Modal { pair, maturity }) =
    { pair = pair
    , maturity = maturity
    }


type Msg
    = Msg


update : Msg -> Modal -> ( Modal, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )
