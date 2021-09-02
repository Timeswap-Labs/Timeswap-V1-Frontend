module Data.Pools exposing
    ( PoolInfo
    , Pools
    , example
    , fromPairFragment
    , fromPoolFragment
    , getFirst
    , toList
    , toListSinglePair
    , toPairs
    )

import Data.Chain exposing (Chain(..))
import Data.Maturity as Maturity exposing (Maturity)
import Data.Pair as Pair exposing (Pair)
import Data.Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Tokens as Tokens exposing (Tokens)
import Sort.Dict as Dict exposing (Dict)
import Time exposing (Posix)


type Pools
    = Pools (Dict Pair (Dict Maturity (Remote PoolInfo))) -- fix in the future


type alias PoolInfo =
    { assetLiquidity : String
    , collateralLiquidity : String
    , apr : String
    , cf : String
    }


fromPairFragment : Tokens -> Pools -> String -> Maybe Pair
fromPairFragment tokens (Pools dict) string =
    string
        |> String.split "&"
        |> (\list ->
                case list of
                    assetString :: collateralString :: _ ->
                        Maybe.map2
                            (\asset collateral ->
                                dict
                                    |> Dict.foldl
                                        (\pair _ accumulator ->
                                            if (pair |> Pair.toAsset) == asset && (pair |> Pair.toCollateral) == collateral then
                                                Just pair

                                            else
                                                accumulator
                                        )
                                        Nothing
                            )
                            (assetString |> Tokens.fromAssetFragment tokens)
                            (collateralString |> Tokens.fromCollateralFragment tokens)
                            |> Maybe.andThen identity

                    _ ->
                        Nothing
           )


fromPoolFragment : Tokens -> Pools -> String -> Maybe Pool
fromPoolFragment tokens ((Pools dict) as pools) string =
    string
        |> String.split "&"
        |> (\list ->
                case list of
                    assetString :: collateralString :: maturityString :: _ ->
                        [ assetString
                        , collateralString
                        ]
                            |> String.join "&"
                            |> fromPairFragment tokens pools
                            |> Maybe.andThen
                                (\pair ->
                                    Maybe.map2
                                        (\innerDict maturity ->
                                            if maturity |> Dict.memberOf innerDict then
                                                { pair = pair
                                                , maturity = maturity
                                                }
                                                    |> Just

                                            else
                                                Nothing
                                        )
                                        (dict |> Dict.get pair)
                                        (maturityString |> Maturity.fromFragment)
                                        |> Maybe.andThen identity
                                )

                    _ ->
                        Nothing
           )



-- dummy data


example : Pools
example =
    Dict.empty Pair.sorter
        |> Dict.insert Pair.daiEthRinkeby
            (Dict.fromList Maturity.sorter
                [ ( Maturity.unix962654400, Loading )
                , ( Maturity.unix1635364800, Success (PoolInfo "1.2M" "1.3K" "12%" "2450.809") )
                , ( Maturity.unix1650889815, Loading )
                ]
            )
        |> Dict.insert Pair.daiMaticRinkeby
            (Dict.fromList Maturity.sorter
                [ ( Maturity.unix1635364800, Loading ) ]
            )
        |> Dict.insert Pair.wethDaiRinkeby
            (Dict.empty Maturity.sorter)
        |> Pools


toPairs : Pools -> List Pair
toPairs (Pools dict) =
    dict |> Dict.keys


toList : Posix -> Pools -> List ( Pair, List ( Maturity, Remote PoolInfo ) )
toList time (Pools dict) =
    dict
        |> Dict.toList
        |> (List.map << Tuple.mapSecond)
            (\innerDict ->
                innerDict
                    |> Dict.keepIf
                        (\maturity _ -> maturity |> Maturity.isActive time)
                    |> Dict.toList
            )


toListSinglePair : Posix -> Pair -> Pools -> List ( Maturity, Remote PoolInfo )
toListSinglePair time pair (Pools dict) =
    dict
        |> Dict.get pair
        |> Maybe.map
            (Dict.keepIf (\maturity _ -> maturity |> Maturity.isActive time))
        |> Maybe.map Dict.toList
        |> Maybe.withDefault []


getFirst : Pools -> Maybe Pair
getFirst (Pools dict) =
    dict
        |> Dict.keys
        |> List.head
