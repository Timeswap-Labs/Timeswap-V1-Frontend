module Data.Positions exposing
    ( ActiveClaimInfo
    , Claim
    , DueInfo
    , DuesInfo
    , MaturedClaimInfo
    , Positions
    , Return
    , example
    , getFirstClaim
    , getFirstDue
    , isClaimEmpty
    , isDuesEmpty
    , toClaimList
    , toClaimListByPair
    , toClaimPools
    , toDueList
    , toDueListByPair
    , toDuePools
    )

import Data.Chain exposing (Chain(..))
import Data.Maturity as Maturity
import Data.Pair as Pair exposing (Pair)
import Data.Pool as Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Status exposing (Status(..))
import Data.TokenId as TokenId exposing (TokenId)
import Sort.Dict as Dict exposing (Dict)
import Time exposing (Posix)


type Positions
    = Positions
        { liquidities : Dict Pool (Status String Return)
        , claims : Dict Pool (Status Claim Return)
        , dues : Dict Pool (Dict TokenId Due)
        }


type alias Return =
    { asset : String
    , collateral : String
    }


type alias Claim =
    { bond : String
    , insurance : String
    }


type alias Due =
    { debt : String
    , collateral : String
    }


type alias ActiveLiquidityInfo =
    { pool : Pool
    , liquidity : String
    }


type alias MaturedLiquidityInfo =
    { pool : Pool
    , return : Maybe Return
    }


type alias ActiveClaimInfo =
    { pool : Pool
    , claim : Claim
    }


type alias MaturedClaimInfo =
    { pool : Pool
    , return : Maybe Return
    }


type alias DuesInfo =
    { pool : Pool
    , listDue : List DueInfo
    }


type alias DueInfo =
    { tokenId : TokenId
    , debt : String
    , collateral : String
    }


toClaimPools : Positions -> List Pool
toClaimPools (Positions { claims }) =
    claims
        |> Dict.keys


toDuePools : Positions -> List Pool
toDuePools (Positions { dues }) =
    dues
        |> Dict.keys


isClaimEmpty : Positions -> Bool
isClaimEmpty (Positions { claims }) =
    claims |> Dict.isEmpty


isDuesEmpty : Positions -> Bool
isDuesEmpty (Positions { dues }) =
    dues |> Dict.isEmpty


getFirstClaim : Posix -> Maybe Pair -> Positions -> Maybe Pool
getFirstClaim time filter positions =
    toClaimList time positions
        |> Tuple.mapBoth
            (\list ->
                list
                    |> List.filter
                        (\{ pool } ->
                            filter
                                |> Maybe.map ((==) pool.pair)
                                |> Maybe.withDefault True
                        )
                    |> List.map .pool
            )
            (\list ->
                list
                    |> List.filter
                        (\{ pool } ->
                            filter
                                |> Maybe.map ((==) pool.pair)
                                |> Maybe.withDefault True
                        )
                    |> List.map .pool
            )
        |> (\( activeList, maturedList ) ->
                maturedList
                    |> List.head
                    |> Maybe.map Just
                    |> Maybe.withDefault (activeList |> List.head)
           )


getFirstDue : Posix -> Maybe Pair -> Positions -> Maybe Pool
getFirstDue time filter positions =
    toDueList time positions
        |> Tuple.mapBoth
            (\list ->
                list
                    |> List.filter
                        (\{ pool } ->
                            filter
                                |> Maybe.map ((==) pool.pair)
                                |> Maybe.withDefault True
                        )
                    |> List.map .pool
            )
            (\list ->
                list
                    |> List.filter
                        (\{ pool } ->
                            filter
                                |> Maybe.map ((==) pool.pair)
                                |> Maybe.withDefault True
                        )
                    |> List.map .pool
            )
        |> (\( activeList, maturedList ) ->
                activeList
                    |> List.head
                    |> Maybe.map Just
                    |> Maybe.withDefault (maturedList |> List.head)
           )


toClaimListByPair : Posix -> Positions -> List ( Pair, Int )
toClaimListByPair time positions =
    toClaimList time positions
        |> Tuple.mapBoth
            (\list ->
                list
                    |> List.foldl
                        (\{ pool } accumulator ->
                            accumulator
                                |> Dict.get pool.pair
                                |> Maybe.map
                                    (\size ->
                                        accumulator
                                            |> Dict.insert pool.pair (size + 1)
                                    )
                                |> Maybe.withDefault
                                    (accumulator |> Dict.insert pool.pair 1)
                        )
                        (Dict.empty Pair.sorter)
            )
            (\list ->
                list
                    |> List.foldl
                        (\{ pool } accumulator ->
                            accumulator
                                |> Dict.get pool.pair
                                |> Maybe.map
                                    (\size ->
                                        accumulator
                                            |> Dict.insert pool.pair (size + 1)
                                    )
                                |> Maybe.withDefault
                                    (accumulator |> Dict.insert pool.pair 1)
                        )
                        (Dict.empty Pair.sorter)
            )
        |> (\( activeDict, maturedDict ) ->
                activeDict
                    |> Dict.foldl
                        (\pair activeSize accumulator ->
                            accumulator
                                |> Dict.get pair
                                |> Maybe.map
                                    (\maturedSize ->
                                        accumulator
                                            |> Dict.insert pair (maturedSize + activeSize)
                                    )
                                |> Maybe.withDefault
                                    (accumulator |> Dict.insert pair activeSize)
                        )
                        maturedDict
           )
        |> Dict.toList


toDueListByPair : Posix -> Positions -> List ( Pair, Int )
toDueListByPair time positions =
    toDueList time positions
        |> Tuple.mapBoth
            (\list ->
                list
                    |> List.foldl
                        (\{ pool } accumulator ->
                            accumulator
                                |> Dict.get pool.pair
                                |> Maybe.map
                                    (\size ->
                                        accumulator
                                            |> Dict.insert pool.pair (size + 1)
                                    )
                                |> Maybe.withDefault
                                    (accumulator |> Dict.insert pool.pair 1)
                        )
                        (Dict.empty Pair.sorter)
            )
            (\list ->
                list
                    |> List.foldl
                        (\{ pool } accumulator ->
                            accumulator
                                |> Dict.get pool.pair
                                |> Maybe.map
                                    (\size ->
                                        accumulator
                                            |> Dict.insert pool.pair (size + 1)
                                    )
                                |> Maybe.withDefault
                                    (accumulator |> Dict.insert pool.pair 1)
                        )
                        (Dict.empty Pair.sorter)
            )
        |> (\( activeDict, maturedDict ) ->
                activeDict
                    |> Dict.foldl
                        (\pair activeSize accumulator ->
                            accumulator
                                |> Dict.get pair
                                |> Maybe.map
                                    (\maturedSize ->
                                        accumulator
                                            |> Dict.insert pair (maturedSize + activeSize)
                                    )
                                |> Maybe.withDefault
                                    (accumulator |> Dict.insert pair activeSize)
                        )
                        maturedDict
           )
        |> Dict.toList


toClaimList : Posix -> Positions -> ( List ActiveClaimInfo, List MaturedClaimInfo )
toClaimList time (Positions { claims }) =
    claims
        |> Dict.partition
            (\{ maturity } _ -> maturity |> Maturity.isActive time)
        |> Tuple.mapBoth
            (\dict ->
                dict
                    |> Dict.foldl
                        (\pool status accumulator ->
                            case status of
                                Active claim ->
                                    accumulator
                                        |> Dict.insert pool claim

                                Matured _ ->
                                    accumulator
                        )
                        (dict
                            |> Dict.dropIf (\_ _ -> True)
                            |> Dict.map
                                (\_ _ ->
                                    { bond = ""
                                    , insurance = ""
                                    }
                                )
                        )
                    |> Dict.toList
                    |> List.map
                        (\( pool, claim ) ->
                            { pool = pool
                            , claim = claim
                            }
                        )
            )
            (\dict ->
                dict
                    |> Dict.map
                        (\_ status ->
                            case status of
                                Active _ ->
                                    Nothing

                                Matured return ->
                                    Just return
                        )
                    |> Dict.toList
                    |> List.map
                        (\( pool, return ) ->
                            { pool = pool
                            , return = return
                            }
                        )
            )


toDueList : Posix -> Positions -> ( List DuesInfo, List DuesInfo )
toDueList time (Positions { dues }) =
    dues
        |> Dict.partition
            (\{ maturity } _ -> maturity |> Maturity.isActive time)
        |> Tuple.mapBoth
            (\dict ->
                dict
                    |> Dict.map
                        (\_ innerDict ->
                            innerDict
                                |> Dict.toList
                                |> List.map
                                    (\( tokenId, { debt, collateral } ) ->
                                        { tokenId = tokenId
                                        , debt = debt
                                        , collateral = collateral
                                        }
                                    )
                        )
                    |> Dict.toList
                    |> List.map
                        (\( pool, listDue ) ->
                            { pool = pool
                            , listDue = listDue
                            }
                        )
            )
            (\dict ->
                dict
                    |> Dict.map
                        (\_ innerDict ->
                            innerDict
                                |> Dict.toList
                                |> List.map
                                    (\( tokenId, { debt, collateral } ) ->
                                        { tokenId = tokenId
                                        , debt = debt
                                        , collateral = collateral
                                        }
                                    )
                        )
                    |> Dict.toList
                    |> List.map
                        (\( pool, listDue ) ->
                            { pool = pool
                            , listDue = listDue
                            }
                        )
            )


example : Positions
example =
    { liquidities = Dict.empty Pool.sorter
    , claims =
        Dict.fromList Pool.sorter
            [ ( { pair = Pair.daiEthRinkeby
                , maturity = Maturity.unix962654400
                }
              , Matured { asset = "1200", collateral = "3.56" }
              )
            , ( { pair = Pair.daiEthRinkeby
                , maturity = Maturity.unix1635364800
                }
              , Active { bond = "1200", insurance = "2.0008" }
              )
            , ( { pair = Pair.daiEthRinkeby
                , maturity = Maturity.unix1650889815
                }
              , Matured { asset = "45", collateral = "3.2" }
              )
            , ( { pair = Pair.daiMaticRinkeby
                , maturity = Maturity.unix1635364800
                }
              , Active { bond = "2133", insurance = "21313" }
              )
            ]
    , dues =
        Dict.fromList Pool.sorter
            [ ( { pair = Pair.daiEthRinkeby
                , maturity = Maturity.unix962654400
                }
              , Dict.fromList TokenId.sorter
                    (TokenId.exampleList
                        |> List.map (\tokenId -> ( tokenId, Due "3000" "200" ))
                    )
              )
            , ( { pair = Pair.daiEthRinkeby
                , maturity = Maturity.unix1635364800
                }
              , Dict.fromList TokenId.sorter
                    (TokenId.exampleList
                        |> List.map (\tokenId -> ( tokenId, Due "3000" "200" ))
                    )
              )
            , ( { pair = Pair.daiEthRinkeby
                , maturity = Maturity.unix1650889815
                }
              , Dict.fromList TokenId.sorter
                    (TokenId.exampleList
                        |> List.map (\tokenId -> ( tokenId, Due "3000" "200" ))
                    )
              )
            , ( { pair = Pair.daiEthRinkeby
                , maturity = Maturity.unix962654400
                }
              , Dict.fromList TokenId.sorter
                    (TokenId.exampleList
                        |> List.map (\tokenId -> ( tokenId, Due "3000" "200" ))
                    )
              )
            ]
    }
        |> Positions
