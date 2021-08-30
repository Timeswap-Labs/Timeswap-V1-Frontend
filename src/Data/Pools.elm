module Data.Pools exposing
    ( Pool
    , PoolInfo
    , Pools
    , getFirst
    , getSize
    , toList
    , toListSinglePool
    , toPairs
    , whitelist
    )

import Data.Chain exposing (Chain(..))
import Data.Maturity as Maturity exposing (Maturity)
import Data.Pair as Pair exposing (Pair)
import Data.Remote exposing (Remote(..))
import Sort.Dict as Dict exposing (Dict)
import Time exposing (Posix)


type Pools
    = Pools (Dict Pair (Dict Maturity (Remote Pool))) -- fix in the future


type alias Pool =
    { assetLiquidity : String
    , collateralLiquidity : String
    , apr : String
    , cf : String
    }


type alias PoolInfo =
    { maturity : Maturity
    , pool : Remote Pool
    }


whitelist : Chain -> Pools
whitelist chain =
    (case chain of
        Mainnet ->
            Dict.empty (Pair.sorter chain)

        Rinkeby ->
            Dict.empty (Pair.sorter chain)
                |> Dict.insert Pair.daiEthRinkeby
                    (Dict.fromList Maturity.sorter
                        [ ( Maturity.unix962654400, Loading )
                        , ( Maturity.unix1635364800, Success (Pool "1.2M" "1.3K" "12%" "2450.809") )
                        , ( Maturity.unix1650889815, Loading )
                        ]
                    )
                |> Dict.insert Pair.daiMaticRinkeby
                    (Dict.fromList Maturity.sorter
                        [ ( Maturity.unix1635364800, Loading ) ]
                    )
                |> Dict.insert Pair.wethDaiRinkeby
                    (Dict.empty Maturity.sorter)
    )
        |> Pools


toPairs : Pools -> List Pair
toPairs (Pools dict) =
    dict
        |> Dict.keys


toList : Pools -> List ( Pair, List PoolInfo )
toList (Pools dict) =
    dict
        |> Dict.toList
        |> (List.map << Tuple.mapSecond)
            (\innerDict ->
                innerDict
                    |> Dict.toList
                    |> List.map
                        (\( maturity, pool ) ->
                            { maturity = maturity
                            , pool = pool
                            }
                        )
            )


toListSinglePool : Pair -> Pools -> List PoolInfo
toListSinglePool pair (Pools dict) =
    dict
        |> Dict.get pair
        |> Maybe.map Dict.toList
        |> (Maybe.map << List.map)
            (\( maturity, pool ) ->
                { maturity = maturity
                , pool = pool
                }
            )
        |> Maybe.withDefault []


getSize : Posix -> Pair -> Pools -> Int
getSize time pair (Pools dict) =
    dict
        |> Dict.get pair
        |> Maybe.map (Dict.keepIf (\maturity _ -> maturity |> Maturity.isActive time))
        |> Maybe.map Dict.size
        |> Maybe.withDefault 0


getFirst : Pools -> Maybe Pair
getFirst (Pools dict) =
    dict
        |> Dict.keys
        |> List.head
