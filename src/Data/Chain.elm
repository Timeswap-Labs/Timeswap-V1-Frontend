module Data.Chain exposing
    ( Chain(..)
    , decoder
    , encode
    , sorter
    , toChainId
    , toEtherscan
    , toQueryParameter
    , toString
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Sort exposing (Sorter)
import Url.Builder as Builder exposing (QueryParameter)


type Chain
    = Chain
        { chainId : Int
        , name : String
        , etherscan : String
        }


decoder : Decoder Chain
decoder =
    Decode.succeed
        (\chainId name etherscan ->
            { chainId = chainId
            , name = name
            , etherscan = etherscan
            }
                |> Chain
        )
        |> Pipeline.required "chainId" Decode.int
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "etherscan" Decode.string


encode : Chain -> Value
encode (Chain { chainId, name, etherscan }) =
    [ ( "chainId", chainId |> Encode.int )
    , ( "name", name |> Encode.string )
    , ( "etherscan", etherscan |> Encode.string )
    ]
        |> Encode.object


sorter : Sorter Chain
sorter =
    Sort.increasing
        |> Sort.by (\(Chain { chainId }) -> chainId)


toChainId : Chain -> Int
toChainId (Chain { chainId }) =
    chainId


toString : Chain -> String
toString (Chain { name }) =
    name


toEtherscan : Chain -> String
toEtherscan (Chain { etherscan }) =
    etherscan


toQueryParameter : Chain -> QueryParameter
toQueryParameter chain =
    chain
        |> toChainId
        |> Builder.int "chainId"
