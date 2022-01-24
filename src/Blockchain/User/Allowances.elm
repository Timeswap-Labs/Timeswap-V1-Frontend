module Blockchain.User.Allowances exposing
    ( Allowances
    , decoder
    , encode
    , encodeSingle
    , hasEnough
    , init
    , subscriptions
    , update
    )

import Data.Address as Address exposing (Address)
import Data.Chain as Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Remote as Remote exposing (Remote(..))
import Data.Uint as Uint exposing (Uint)
import Data.Web exposing (Web)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Sort.Dict as Dict exposing (Dict)
import Time exposing (Posix)


type alias Allowances =
    Dict ERC20 (Web Uint)


type alias Answer =
    { chain : Chain
    , address : Address
    , allowances : Allowances
    }


init :
    Chains
    -> Chain
    -> Allowances
init chains chain =
    chains
        |> Chains.toERC20List chain
        |> List.map (\erc20 -> ( erc20, Remote.loading ))
        |> Dict.fromList ERC20.sorter


encode : Chains -> Chain -> Address -> Value
encode chains chain address =
    [ ( "chain", chain |> Chain.encode )
    , ( "address", address |> Address.encode )
    , ( "erc20s"
      , chains
            |> Chains.toERC20List chain
            |> Encode.list ERC20.encode
      )
    ]
        |> Encode.object


encodeSingle : Chain -> Address -> ERC20 -> Value
encodeSingle chain address erc20 =
    [ ( "chain", chain |> Chain.encode )
    , ( "address", address |> Address.encode )
    , ( "erc20s", [ erc20 ] |> Encode.list ERC20.encode )
    ]
        |> Encode.object


decoder : Decoder Answer
decoder =
    Decode.succeed
        (\chain address erc20s allowances ->
            { chain = chain
            , address = address
            , allowances =
                List.map2
                    (\erc20 allowance ->
                        ( erc20, allowance |> Success )
                    )
                    erc20s
                    allowances
                    |> Dict.fromList ERC20.sorter
            }
        )
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "address" Address.decoder
        |> Pipeline.required "erc20s" (ERC20.decoder |> Decode.list)
        |> Pipeline.required "allowances" (Uint.decoder |> Decode.list)


hasEnough : ERC20 -> Uint -> Allowances -> Bool
hasEnough erc20 amount allowances =
    allowances
        |> Dict.get erc20
        |> (Maybe.map << Remote.map) (Uint.hasEnough amount)
        |> (Maybe.map << Remote.withDefault) False
        |> Maybe.withDefault False


update : Posix -> Allowances -> Allowances
update posix balances =
    balances
        |> Dict.toList
        |> (List.map << Tuple.mapSecond) (Remote.update posix)
        |> Dict.fromList ERC20.sorter


subscriptions : (Posix -> msg) -> Allowances -> Sub msg
subscriptions tick balances =
    balances
        |> Dict.values
        |> List.map (Remote.subscriptions tick)
        |> Sub.batch
