module Data.Chain exposing
    ( Chain(..)
    , decoder
    , encode
    , sorter
    , toBlockExplorerUrl
    , toChainId
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
        , rpcUrl : String
        , blockExplorerUrl : String
        , nftExplorerUrl : String
        }


decoder : Decoder Chain
decoder =
    Decode.succeed
        (\chainId name rpcUrl blockExplorerUrl nftExplorerUrl ->
            { chainId = chainId
            , name = name
            , rpcUrl = rpcUrl
            , blockExplorerUrl = blockExplorerUrl
            , nftExplorerUrl = nftExplorerUrl
            }
                |> Chain
        )
        |> Pipeline.required "chainId" Decode.int
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "rpcUrl" Decode.string
        |> Pipeline.required "blockExplorerUrl" Decode.string
        |> Pipeline.required "nftExplorerUrl" Decode.string


encode : Chain -> Value
encode (Chain { chainId, name, rpcUrl, blockExplorerUrl, nftExplorerUrl }) =
    [ ( "chainId", chainId |> Encode.int )
    , ( "name", name |> Encode.string )
    , ( "rpcUrl", rpcUrl |> Encode.string )
    , ( "blockExplorerUrl", blockExplorerUrl |> Encode.string )
    , ( "nftExplorerUrl", nftExplorerUrl |> Encode.string )
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


toBlockExplorerUrl : Chain -> String
toBlockExplorerUrl (Chain { blockExplorerUrl }) =
    blockExplorerUrl


toQueryParameter : Chain -> QueryParameter
toQueryParameter chain =
    chain
        |> toChainId
        |> Builder.int "chainId"
