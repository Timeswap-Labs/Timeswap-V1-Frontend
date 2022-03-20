module Data.ERC20 exposing
    ( ERC20
    , Flag
    , compare
    , decoder
    , decoderSet
    , encode
    , encodeAddress
    , fromString
    , init
    , initWrapper
    , sorter
    , sorterWrapperFirst
    , toAddress
    , toDecimals
    , toName
    , toString
    , toSymbol
    , zero
    )

import Data.Address as Address exposing (Address)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Sort exposing (Sorter)
import Sort.Set as Set exposing (Set)


type ERC20
    = ERC20
        { address : Address
        , name : String
        , symbol : String
        , decimals : Int
        }


type alias Flag =
    { address : String
    , name : String
    , symbol : String
    , decimals : Int
    }


init : Flag -> Maybe ERC20
init ({ name, symbol, decimals } as flag) =
    flag.address
        |> Address.fromString
        |> Maybe.map
            (\address ->
                { address = address
                , name = name
                , symbol = symbol
                , decimals = decimals
                }
                    |> ERC20
            )


initWrapper : Flag -> ERC20
initWrapper { address, name, symbol, decimals } =
    { address =
        address
            |> Address.fromString
            |> Maybe.withDefault Address.zero
    , name = name
    , symbol = symbol
    , decimals = decimals
    }
        |> ERC20


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


decoder : Decoder ERC20
decoder =
    Decode.succeed
        (\address name symbol decimals ->
            { address = address
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


decoderSet : Decoder (Set ERC20)
decoderSet =
    decoder
        |> Decode.list
        |> Decode.map (Set.fromList sorter)


encode : ERC20 -> Value
encode (ERC20 { address, name, symbol, decimals }) =
    [ ( "address", address |> Address.encode )
    , ( "name", name |> Encode.string )
    , ( "symbol", symbol |> Encode.string )
    , ( "decimals", decimals |> Encode.int )
    ]
        |> Encode.object


encodeAddress : ERC20 -> Value
encodeAddress (ERC20 { address }) =
    address |> Address.encode


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


toDecimals : ERC20 -> Int
toDecimals (ERC20 { decimals }) =
    decimals


compare : ERC20 -> ERC20 -> Order
compare (ERC20 erc20a) (ERC20 erc20b) =
    Basics.compare erc20a.symbol erc20b.symbol


compareWrapperFirst : ERC20 -> ERC20 -> ERC20 -> Order
compareWrapperFirst wrapper erc20A erc20B =
    if erc20A == wrapper then
        GT

    else if erc20B == wrapper then
        LT

    else
        compare erc20A erc20B


sorter : Sorter ERC20
sorter =
    Sort.alphabetical
        |> Sort.by (\(ERC20 { symbol }) -> symbol)


sorterWrapperFirst : ERC20 -> Sorter ERC20
sorterWrapperFirst =
    compareWrapperFirst >> Sort.custom


zero : ERC20
zero =
    ERC20
        { address = Address.zero
        , name = ""
        , symbol = ""
        , decimals = 0
        }
