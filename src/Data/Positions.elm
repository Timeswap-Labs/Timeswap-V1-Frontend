module Data.Positions exposing
    ( ActiveClaimInfo
    , Claim
    , DueInfo
    , DuesInfo
    , MaturedClaimInfo
    , Positions
    , Return
    , getClaimReturn
    , getFirstClaim
    , getFirstDue
    , init
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
    , update
    )

import Data.Chain exposing (Chain(..))
import Data.Maturity as Maturity
import Data.Pair as Pair exposing (Pair)
import Data.Pool as Pool exposing (Pool)
import Data.Pools as Pools exposing (Pools)
import Data.Remote exposing (Remote(..))
import Data.Status exposing (Status(..))
import Data.TokenId as TokenId exposing (TokenId)
import Data.Tokens exposing (Tokens)
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline
import Sort.Dict as Dict exposing (Dict)
import Sort.Set as Set exposing (Set)
import Time exposing (Posix)


type Positions
    = Positions
        { liquidities : Dict Pool (Status LiquidityUint LiquidityReturnUint)
        , claims : Dict Pool (Status ClaimUint ReturnUint)
        , dues : Dict Pool (Dict TokenId DueUint)
        }


type alias LiquidityUint =
    Uint


type alias LiquidityReturnUint =
    { liquidity : Uint
    , asset : Uint
    , collateral : Uint
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


decoder : Pools -> Tokens -> Decoder Positions
decoder pools tokens =
    Decode.oneOf
        [ decoderLiquidities pools tokens
        , decoderClaims pools tokens
        , decoderDues pools tokens
        ]
        |> Decode.list
        |> Decode.map
            (\list ->
                list
                    |> List.foldl
                        (\(Positions { liquidities, claims, dues }) (Positions accumulator) ->
                            { liquidities =
                                accumulator.liquidities
                                    |> Dict.insertAll liquidities
                            , claims =
                                accumulator.claims
                                    |> Dict.insertAll claims
                            , dues =
                                accumulator.dues
                                    |> Dict.insertAll dues
                            }
                                |> Positions
                        )
                        ({ liquidities = Dict.empty Pool.sorter
                         , claims = Dict.empty Pool.sorter
                         , dues = Dict.empty Pool.sorter
                         }
                            |> Positions
                        )
            )


decoderLiquidities : Pools -> Tokens -> Decoder Positions
decoderLiquidities pools tokens =
    Decode.succeed
        (\pool status ->
            Dict.singleton Pool.sorter pool status
        )
        |> Pipeline.custom (Pools.decoderPool pools tokens)
        |> Pipeline.custom
            (Decode.oneOf
                [ decoderLiquidityUint |> Decode.map Active
                , decoderLiquidityReturnUint |> Decode.map Matured
                ]
            )
        |> Decode.map
            (\liquidities ->
                { liquidities = liquidities
                , claims = Dict.empty Pool.sorter
                , dues = Dict.empty Pool.sorter
                }
                    |> Positions
            )


decoderLiquidityUint : Decoder LiquidityUint
decoderLiquidityUint =
    Decode.field "liquidity" Uint.decoder


decoderLiquidityReturnUint : Decoder LiquidityReturnUint
decoderLiquidityReturnUint =
    Decode.succeed LiquidityReturnUint
        |> Pipeline.required "liquidity" Uint.decoder
        |> Pipeline.required "assetOut" Uint.decoder
        |> Pipeline.required "collateralOut" Uint.decoder


decoderClaims : Pools -> Tokens -> Decoder Positions
decoderClaims pools tokens =
    Decode.succeed
        (\pool status ->
            Dict.singleton Pool.sorter pool status
        )
        |> Pipeline.custom (Pools.decoderPool pools tokens)
        |> Pipeline.custom
            (Decode.oneOf
                [ decoderReturnUint |> Decode.map Matured
                , decoderClaimUint |> Decode.map Active
                ]
            )
        |> Decode.map
            (\claims ->
                { liquidities = Dict.empty Pool.sorter
                , claims = claims
                , dues = Dict.empty Pool.sorter
                }
                    |> Positions
            )


decoderClaimUint : Decoder ClaimUint
decoderClaimUint =
    Decode.succeed ClaimUint
        |> Pipeline.required "bond" Uint.decoder
        |> Pipeline.required "insurance" Uint.decoder


decoderReturnUint : Decoder ReturnUint
decoderReturnUint =
    Decode.succeed ReturnUint
        |> Pipeline.required "bond" Uint.decoder
        |> Pipeline.required "insurance" Uint.decoder
        |> Pipeline.required "assetOut" Uint.decoder
        |> Pipeline.required "collateralOut" Uint.decoder


decoderDues : Pools -> Tokens -> Decoder Positions
decoderDues pools tokens =
    Decode.succeed
        (\pool dues ->
            Dict.singleton Pool.sorter pool dues
        )
        |> Pipeline.custom (Pools.decoderPool pools tokens)
        |> Pipeline.required "dues" decoderDuesUint
        |> Decode.map
            (\dues ->
                { liquidities = Dict.empty Pool.sorter
                , claims = Dict.empty Pool.sorter
                , dues = dues
                }
                    |> Positions
            )


decoderDueUint : Decoder DueUint
decoderDueUint =
    Decode.succeed DueUint
        |> Pipeline.required "debt" Uint.decoder
        |> Pipeline.required "collateral" Uint.decoder


decoderDuesUint : Decoder (Dict TokenId DueUint)
decoderDuesUint =
    Decode.succeed
        (\tokenId due ->
            Dict.singleton TokenId.sorter tokenId due
        )
        |> Pipeline.required "id" TokenId.decoder
        |> Pipeline.custom decoderDueUint
        |> Decode.list
        |> Decode.map
            (\list ->
                list
                    |> List.foldl
                        (\dict accumulator ->
                            accumulator
                                |> Dict.insertAll dict
                        )
                        (Dict.empty TokenId.sorter)
            )


init : Pools -> Tokens -> Value -> Remote () Positions
init pools tokens value =
    value
        |> Decode.decodeValue (decoder pools tokens)
        |> (\result ->
                case result of
                    Ok positions ->
                        Success positions

                    Err _ ->
                        Loading
           )


update : Pools -> Tokens -> Value -> Positions -> Positions
update pools tokens value (Positions positions) =
    value
        |> Decode.decodeValue (decoder pools tokens)
        |> (\result ->
                case result of
                    Ok (Positions { liquidities, claims, dues }) ->
                        { liquidities =
                            positions.liquidities
                                |> Dict.insertAll liquidities
                        , claims =
                            positions.claims
                                |> Dict.insertAll claims
                        , dues =
                            positions.dues
                                |> Dict.insertAll dues
                        }
                            |> Positions

                    Err _ ->
                        Positions positions
           )


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
