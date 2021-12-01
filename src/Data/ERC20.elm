module Data.ERC20 exposing
    ( ERC20
    , axie
    , compare
    , daiRinkeby
    , decoder
    , doge
    , encode
    , fromString
    , matic
    , maticRinkeby
    , shiba
    , sorter
    , toAddress
    , toDecimals
    , toKey
    , toName
    , toString
    , toSymbol
    , usdc
    , wethRinkeby
    )

import Data.Address as Address exposing (Address)
import Data.Chain exposing (Chain(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode exposing (Value)
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


doge : ERC20
doge =
    { id = 2, address = Address.doge, name = "Dogecoin", symbol = "TS-DOGE", decimals = 18 } |> ERC20


shiba : ERC20
shiba =
    { id = 3, address = Address.shiba, name = "Shiba Inu", symbol = "TS-SHIB", decimals = 18 } |> ERC20


usdc : ERC20
usdc =
    { id = 4, address = Address.usdc, name = "USDC", symbol = "TS-USDC", decimals = 18 } |> ERC20


axie : ERC20
axie =
    { id = 5, address = Address.axie, name = "Axie Infinity", symbol = "TS-AXS", decimals = 18 } |> ERC20


matic : ERC20
matic =
    { id = 6, address = Address.matic, name = "Polygon", symbol = "TS-MATIC", decimals = 18 } |> ERC20


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


decoder : Int -> Decoder ERC20
decoder id =
    Decode.succeed
        (\address name symbol decimals ->
            { id = id
            , address = address
            , name = name
            , symbol = symbol
            , decimals = decimals
            }
                |> ERC20
        )
        |> Pipeline.required "address" Address.decoder
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "symbol" Decode.string
        |> Pipeline.required "decimals" Decode.int


encode : ERC20 -> Value
encode (ERC20 { address }) =
    address |> Address.encode


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
