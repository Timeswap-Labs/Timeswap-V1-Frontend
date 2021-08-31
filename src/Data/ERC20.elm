module Data.ERC20 exposing
    ( ERC20
    , compare
    , daiRinkeby
    , fromString
    , maticRinkeby
    , sorter
    , toName
    , toString
    , toSymbol
    , wethRinkeby
    , whitelist
    )

import Data.Address as Address exposing (Address)
import Data.Chain exposing (Chain(..))
import Sort exposing (Sorter)
import Sort.Set as Set exposing (Set)


type ERC20
    = ERC20
        { address : Address
        , name : String
        , symbol : String
        , decimals : Int
        }


fromString : Chain -> String -> Maybe ERC20
fromString chain string =
    string
        |> Address.fromString
        |> Maybe.andThen
            (\address ->
                whitelist chain
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


toAddress : ERC20 -> Address
toAddress (ERC20 { address }) =
    address


toName : ERC20 -> String
toName (ERC20 { name }) =
    name


toSymbol : ERC20 -> String
toSymbol (ERC20 { symbol }) =
    symbol


whitelist : Chain -> Set ERC20
whitelist chain =
    (case chain of
        Mainnet ->
            []

        Rinkeby ->
            [ daiRinkeby
            , maticRinkeby
            , wethRinkeby
            ]
    )
        |> Set.fromList (sorter chain)


toRank : Chain -> ERC20 -> Int
toRank chain erc20 =
    case chain of
        Mainnet ->
            0

        Rinkeby ->
            if erc20 == daiRinkeby then
                0

            else if erc20 == maticRinkeby then
                1

            else if erc20 == wethRinkeby then
                2

            else
                3


compare : Chain -> ERC20 -> ERC20 -> Order
compare chain ((ERC20 erc20a) as erc20A) ((ERC20 erc20b) as erc20B) =
    Basics.compare (erc20A |> toRank chain) (erc20B |> toRank chain)
        |> (\order ->
                case order of
                    EQ ->
                        Address.compare erc20a.address erc20b.address

                    _ ->
                        order
           )


sorter : Chain -> Sorter ERC20
sorter chain =
    Sort.custom (compare chain)


daiRinkeby : ERC20
daiRinkeby =
    ERC20
        { address = Address.daiRinkeby
        , name = "DAI Stablecoin"
        , symbol = "DAI"
        , decimals = 18
        }


maticRinkeby : ERC20
maticRinkeby =
    ERC20
        { address = Address.maticRinkeby
        , name = "Matic Token"
        , symbol = "MATIC"
        , decimals = 18
        }


wethRinkeby : ERC20
wethRinkeby =
    ERC20
        { address = Address.wethRinkeby
        , name = "Wrapped Ether"
        , symbol = "WETH"
        , decimals = 18
        }
