module Data.Positions exposing
    ( ActiveClaimInfo
    , Claim
    , DueInfo
    , DuesInfo
    , MaturedClaimInfo
    , Positions
    , Return
    , example
    , getClaimReturn
    , getFirstClaim
    , getFirstDue
    , isClaimEmpty
    , isDuesEmpty
    , isOwner
    , toClaimList
    , toClaimListByPair
    , toClaimPools
    , toClaimTransaction
    , toDueList
    , toDueListByPair
    , toDuePools
    , toDueTransaction
    )

import Data.Chain exposing (Chain(..))
import Data.Maturity as Maturity
import Data.Pair as Pair exposing (Pair)
import Data.Pool as Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Status exposing (Status(..))
import Data.TokenId as TokenId exposing (TokenId)
import Data.Uint as Uint exposing (Uint)
import Sort.Dict as Dict exposing (Dict)
import Sort.Set as Set exposing (Set)
import Time exposing (Posix)


type Positions
    = Positions
        { liquidities : Dict Pool (Status String Return)
        , claims : Dict Pool (Status ClaimUint ReturnUint)
        , dues : Dict Pool (Dict TokenId DueUint)
        }


type alias ClaimUint =
    { bond : Uint
    , insurance : Uint
    }


type alias ReturnUint =
    { bond : Uint
    , insurance : Uint
    , asset : Uint
    , collateral : Uint
    }


type alias DueUint =
    { debt : Uint
    , collateral : Uint
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


isOwner : Pool -> Set TokenId -> Positions -> Bool
isOwner pool set (Positions { dues }) =
    dues
        |> Dict.get pool
        |> Maybe.map
            (\dict ->
                set
                    |> Set.foldl
                        (\tokenId accumulator ->
                            (tokenId |> Dict.memberOf dict)
                                && accumulator
                        )
                        True
            )
        |> Maybe.withDefault False


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
                        (\({ pair } as pool) status accumulator ->
                            case status of
                                Active { bond, insurance } ->
                                    accumulator
                                        |> Dict.insert pool
                                            { bond =
                                                bond
                                                    |> Uint.toAmount (pair |> Pair.toAsset)
                                            , insurance =
                                                insurance
                                                    |> Uint.toAmount (pair |> Pair.toCollateral)
                                            }

                                Matured _ ->
                                    accumulator
                        )
                        (Dict.empty Pool.sorter)
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
                        (\{ pair } status ->
                            case status of
                                Active _ ->
                                    Nothing

                                Matured { asset, collateral } ->
                                    { asset =
                                        asset
                                            |> Uint.toAmount (pair |> Pair.toAsset)
                                    , collateral =
                                        collateral
                                            |> Uint.toAmount (pair |> Pair.toCollateral)
                                    }
                                        |> Just
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
                        (\{ pair } innerDict ->
                            innerDict
                                |> Dict.toList
                                |> List.map
                                    (\( tokenId, { debt, collateral } ) ->
                                        { tokenId = tokenId
                                        , debt =
                                            debt
                                                |> Uint.toAmount (pair |> Pair.toAsset)
                                        , collateral =
                                            collateral
                                                |> Uint.toAmount (pair |> Pair.toCollateral)
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
                        (\{ pair } innerDict ->
                            innerDict
                                |> Dict.toList
                                |> List.map
                                    (\( tokenId, { debt, collateral } ) ->
                                        { tokenId = tokenId
                                        , debt =
                                            debt
                                                |> Uint.toAmount (pair |> Pair.toAsset)
                                        , collateral =
                                            collateral
                                                |> Uint.toAmount (pair |> Pair.toCollateral)
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


getClaimReturn : Pool -> Positions -> Maybe Return
getClaimReturn ({ pair } as pool) (Positions { claims }) =
    claims
        |> Dict.get pool
        |> Maybe.andThen
            (\status ->
                case status of
                    Active _ ->
                        Nothing

                    Matured { asset, collateral } ->
                        { asset = asset |> Uint.toAmount (pair |> Pair.toAsset)
                        , collateral = collateral |> Uint.toAmount (pair |> Pair.toCollateral)
                        }
                            |> Just
            )


toClaimTransaction : Pool -> Positions -> Maybe ClaimUint
toClaimTransaction pool (Positions { claims }) =
    claims
        |> Dict.get pool
        |> Maybe.andThen
            (\status ->
                case status of
                    Active _ ->
                        Nothing

                    Matured { bond, insurance } ->
                        { bond = bond
                        , insurance = insurance
                        }
                            |> Just
            )


toDueTransaction : Pool -> Set TokenId -> Positions -> Maybe (Dict TokenId Uint)
toDueTransaction pool set (Positions { dues }) =
    dues
        |> Dict.get pool
        |> Maybe.andThen
            (\dict ->
                set
                    |> Set.foldl
                        (\tokenId accumulator ->
                            dict
                                |> Dict.get tokenId
                                |> Maybe.map .debt
                                |> Maybe.andThen
                                    (\debt ->
                                        accumulator
                                            |> Maybe.map (Dict.insert tokenId debt)
                                    )
                        )
                        (Dict.empty TokenId.sorter |> Just)
            )


example : Positions
example =
    { liquidities = Dict.empty Pool.sorter
    , claims =
        Dict.fromList Pool.sorter
            [ ( { pair = Pair.daiEthRinkeby
                , maturity = Maturity.unix962654400
                }
              , Matured
                    { bond = Uint.Uint "21213131"
                    , insurance = Uint.Uint "213141212"
                    , asset = Uint.Uint "1200"
                    , collateral = Uint.Uint "356"
                    }
              )
            , ( { pair = Pair.daiEthRinkeby
                , maturity = Maturity.unix1635364800
                }
              , Active { bond = Uint.Uint "1200", insurance = Uint.Uint "20008" }
              )
            , ( { pair = Pair.daiEthRinkeby
                , maturity = Maturity.unix1650889815
                }
              , Matured
                    { bond = Uint.Uint "21213131"
                    , insurance = Uint.Uint "213141212"
                    , asset = Uint.Uint "45"
                    , collateral = Uint.Uint "32"
                    }
              )
            , ( { pair = Pair.daiMaticRinkeby
                , maturity = Maturity.unix1635364800
                }
              , Active { bond = Uint.Uint "2133", insurance = Uint.Uint "21313" }
              )
            ]
    , dues =
        Dict.fromList Pool.sorter
            [ ( { pair = Pair.daiEthRinkeby
                , maturity = Maturity.unix962654400
                }
              , Dict.fromList TokenId.sorter
                    (TokenId.exampleList
                        |> List.map (\tokenId -> ( tokenId, DueUint (Uint.Uint "3000") (Uint.Uint "200") ))
                    )
              )
            , ( { pair = Pair.daiEthRinkeby
                , maturity = Maturity.unix1635364800
                }
              , Dict.fromList TokenId.sorter
                    (TokenId.exampleList
                        |> List.map (\tokenId -> ( tokenId, DueUint (Uint.Uint "3000") (Uint.Uint "200") ))
                    )
              )
            , ( { pair = Pair.daiEthRinkeby
                , maturity = Maturity.unix1650889815
                }
              , Dict.fromList TokenId.sorter
                    (TokenId.exampleList
                        |> List.map (\tokenId -> ( tokenId, DueUint (Uint.Uint "3000") (Uint.Uint "200") ))
                    )
              )
            , ( { pair = Pair.daiEthRinkeby
                , maturity = Maturity.unix962654400
                }
              , Dict.fromList TokenId.sorter
                    (TokenId.exampleList
                        |> List.map (\tokenId -> ( tokenId, DueUint (Uint.Uint "3000") (Uint.Uint "200") ))
                    )
              )
            ]
    }
        |> Positions
