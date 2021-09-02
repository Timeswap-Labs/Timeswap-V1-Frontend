module Data.Allowances exposing (..)

import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Token as Token exposing (Token)
import Data.Uint as Uint exposing (Uint)
import Sort.Dict as Dict exposing (Dict)


type Allowances
    = Allowances (Dict ERC20 Uint)


hasEnough : Token -> String -> Allowances -> Bool
hasEnough token string (Allowances dict) =
    case token of
        Token.ETH ->
            True

        Token.ERC20 erc20 ->
            Maybe.map2 Uint.compare
                (dict |> Dict.get erc20)
                (string |> Uint.fromAmount token)
                |> Maybe.map
                    (\order ->
                        case order of
                            LT ->
                                False

                            _ ->
                                True
                    )
                |> Maybe.withDefault False


example : Allowances
example =
    List.map2 Tuple.pair
        [ ERC20.daiRinkeby
        , ERC20.maticRinkeby
        , ERC20.wethRinkeby
        ]
        Uint.example
        |> Dict.fromList ERC20.sorter
        |> Allowances
