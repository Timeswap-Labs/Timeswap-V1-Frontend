module Data.ERC20s exposing (ERC20s, Flags, encode, init)

import Data.ERC20 as ERC20 exposing (ERC20)
import Json.Encode as Encode exposing (Value)
import Sort.Set as Set exposing (Set)


type alias ERC20s =
    Set ERC20


type alias Flags =
    List ERC20.Flag


init : Flags -> Set ERC20
init list =
    list
        |> List.foldl
            (\flag accumulator ->
                flag
                    |> ERC20.init
                    |> Maybe.map
                        (\erc20 ->
                            accumulator
                                |> Set.insert erc20
                        )
                    |> Maybe.withDefault accumulator
            )
            (Set.empty ERC20.sorter)


encode : ERC20s -> Value
encode erc20s =
    erc20s
        |> Set.toList
        |> Encode.list ERC20.encode
