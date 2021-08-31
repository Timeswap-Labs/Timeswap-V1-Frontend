module Data.Balances exposing (BalanceInfo, Balances, example, isEmpty, toList)

import Data.Chain exposing (Chain(..))
import Data.ERC20 as ERC20
import Data.Token as Token exposing (Token)
import Sort.Dict as Dict exposing (Dict)


type Balances
    = Balances (Dict Token String)


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
                , balance = balance
                }
            )


example : Balances
example =
    Dict.fromList (Token.sorter Rinkeby)
        [ ( Token.ETH, "26.56" )
        , ( Token.ERC20 ERC20.daiRinkeby, "388870.2131" )
        , ( Token.ERC20 ERC20.maticRinkeby, "12.00013131312" )
        , ( Token.ERC20 ERC20.wethRinkeby, "0.088313" )
        ]
        |> Balances
