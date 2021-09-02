module Data.Balances exposing (BalanceInfo, Balances, example, get, hasEnough, isEmpty, toList)

import Data.ERC20 as ERC20
import Data.Token as Token exposing (Token)
import Data.Uint as Uint exposing (Uint)
import Sort.Dict as Dict exposing (Dict)


type Balances
    = Balances (Dict Token Uint)


type alias BalanceInfo =
    { token : Token
    , balance : String
    }


isEmpty : Balances -> Bool
isEmpty (Balances dict) =
    dict |> Dict.isEmpty


toList : Balances -> List BalanceInfo
toList (Balances dict) =
    dict
        |> Dict.toList
        |> List.map
            (\( token, balance ) ->
                { token = token
                , balance =
                    balance
                        |> Uint.toAmount (token |> Token.toDecimals)
                }
            )


get : Token -> Balances -> String
get token (Balances dict) =
    dict
        |> Dict.get token
        |> Maybe.map (Uint.toAmount (token |> Token.toDecimals))
        |> Maybe.withDefault "0"


hasEnough : Token -> String -> Balances -> Bool
hasEnough token string (Balances dict) =
    Maybe.map2 Uint.compare
        (dict |> Dict.get token)
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


example : Balances
example =
    List.map2 Tuple.pair
        [ Token.ETH
        , Token.ERC20 ERC20.daiRinkeby
        , Token.ERC20 ERC20.maticRinkeby
        , Token.ERC20 ERC20.wethRinkeby
        ]
        Uint.example
        |> Dict.fromList Token.sorter
        |> Balances
