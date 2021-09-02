module Data.ERC20 exposing
    ( ERC20
    , compare
    , daiRinkeby
    , example
    , fromString
    , maticRinkeby
    , sorter
    , toDecimals
    , toKey
    , toName
    , toString
    , toSymbol
    , wethRinkeby
    )

import Data.Address as Address exposing (Address)
import Data.Chain exposing (Chain(..))
import Sort exposing (Sorter)
import Sort.Set as Set exposing (Set)


type ERC20
    = ERC20
        { id : Int
        , address : Address
        , name : String
        , symbol : String
        , decimals : Int
        }


fromString : Set ERC20 -> String -> Maybe ERC20
fromString set string =
    string
        |> Address.fromString
        |> Maybe.andThen
            (\address ->
                set
                    |> Set.foldl
                        (\erc20 accumulator ->
                            if (erc20 |> toAddress) == address then
                                Just erc20

                            else
                                accumulator
                        )
                        Nothing
            )


toString : ERC20 -> String
toString erc20 =
    erc20
        |> toAddress
        |> Address.toString


toKey : ERC20 -> String
toKey =
    toString


toAddress : ERC20 -> Address
toAddress (ERC20 { address }) =
    address


toName : ERC20 -> String
toName (ERC20 { name }) =
    name


toSymbol : ERC20 -> String
toSymbol (ERC20 { symbol }) =
    symbol


toDecimals : ERC20 -> Int
toDecimals (ERC20 { decimals }) =
    decimals


example : Set ERC20
example =
    Set.fromList sorter
        [ daiRinkeby
        , maticRinkeby
        , wethRinkeby
        ]


compare : ERC20 -> ERC20 -> Order
compare (ERC20 erc20a) (ERC20 erc20b) =
    Basics.compare erc20a.id erc20b.id


sorter : Sorter ERC20
sorter =
    Sort.custom compare


daiRinkeby : ERC20
daiRinkeby =
    ERC20
        { id = 0
        , address = Address.daiRinkeby
        , name = "DAI Stablecoin"
        , symbol = "DAI"
        , decimals = 18
        }


maticRinkeby : ERC20
maticRinkeby =
    ERC20
        { id = 1
        , address = Address.maticRinkeby
        , name = "Matic Token"
        , symbol = "MATIC"
        , decimals = 18
        }


wethRinkeby : ERC20
wethRinkeby =
    ERC20
        { id = 2
        , address = Address.wethRinkeby
        , name = "Wrapped Ether"
        , symbol = "WETH"
        , decimals = 18
        }
