module Data.Positions exposing
    ( ActiveClaimInfo
    , Claim
    , MaturedClaimInfo
    , Positions
    , Return
    , example
    , isClaimEmpty
    , isDuesEmpty
    , toClaimList
    , toClaimPools
    , toDuePools
    )

import Data.Chain exposing (Chain(..))
import Data.Maturity as Maturity exposing (Maturity)
import Data.Pair as Pair exposing (Pair)
import Data.Remote exposing (Remote(..))
import Data.Status exposing (Status(..))
import Data.TokenId exposing (TokenId)
import Sort.Dict as Dict exposing (Dict)
import Time exposing (Posix)


type Positions
    = Positions
        { liquidities : Dict Pair (Dict Maturity (Status String Return))
        , claims : Dict Pair (Dict Maturity (Status Claim Return))
        , dues : Dict Pair (Dict Maturity (Dict TokenId Due))
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
    { pair : Pair
    , maturity : Maturity
    , liquidity : String
    }


type alias MaturedLiquidityInfo =
    { pair : Pair
    , maturity : Maturity
    , return : Maybe Return
    }


type alias ActiveClaimInfo =
    { pair : Pair
    , maturity : Maturity
    , claim : Claim
    }


type alias MaturedClaimInfo =
    { pair : Pair
    , maturity : Maturity
    , return : Maybe Return
    }


type alias DuesInfo =
    { pair : Pair
    , maturity : Maturity
    , listDue : List DueInfo
    }


type alias DueInfo =
    { tokenId : TokenId
    , debt : String
    , collateral : String
    }


toClaimPools : Positions -> List ( Pair, Maturity )
toClaimPools (Positions { claims }) =
    claims
        |> Dict.toList
        |> List.concatMap
            (\( pair, innerDict ) ->
                innerDict
                    |> Dict.keys
                    |> List.map
                        (\maturity ->
                            ( pair, maturity )
                        )
            )


toDuePools : Positions -> List ( Pair, Maturity )
toDuePools (Positions { dues }) =
    dues
        |> Dict.toList
        |> List.concatMap
            (\( pair, innerDict ) ->
                innerDict
                    |> Dict.keys
                    |> List.map
                        (\maturity ->
                            ( pair, maturity )
                        )
            )


isClaimEmpty : Positions -> Bool
isClaimEmpty (Positions { claims }) =
    claims |> Dict.isEmpty


isDuesEmpty : Positions -> Bool
isDuesEmpty (Positions { dues }) =
    dues |> Dict.isEmpty


toClaimList : Posix -> Positions -> ( List ActiveClaimInfo, List MaturedClaimInfo )
toClaimList time (Positions { claims }) =
    claims
        |> Dict.map
            (\_ dict ->
                dict
                    |> Dict.partition
                        (\maturity _ ->
                            maturity |> Maturity.isActive time
                        )
            )
        |> Dict.foldl
            (\pair ( activeDict, maturedDict ) ( activeAccumulator, maturedAccumulator ) ->
                ( activeAccumulator |> Dict.insert pair activeDict
                , maturedAccumulator |> Dict.insert pair maturedDict
                )
            )
            ( claims |> Dict.dropIf (\_ _ -> True)
            , claims |> Dict.dropIf (\_ _ -> True)
            )
        |> Tuple.mapBoth
            (\dict ->
                dict
                    |> Dict.map
                        (\_ innerDict ->
                            innerDict
                                |> Dict.foldl
                                    (\maturity status accumulator ->
                                        case status of
                                            Active claim ->
                                                accumulator
                                                    |> Dict.insert maturity claim

                                            Matured _ ->
                                                accumulator
                                    )
                                    (Dict.empty Maturity.sorter)
                        )
                    |> Dict.dropIf (\_ innerDict -> innerDict |> Dict.isEmpty)
                    |> Dict.toList
                    |> List.concatMap
                        (\( pair, innerDict ) ->
                            innerDict
                                |> Dict.toList
                                |> List.map
                                    (\( maturity, claim ) ->
                                        { pair = pair
                                        , maturity = maturity
                                        , claim = claim
                                        }
                                    )
                        )
            )
            (\dict ->
                dict
                    |> Dict.map
                        (\_ innerDict ->
                            innerDict
                                |> Dict.map
                                    (\_ status ->
                                        case status of
                                            Active _ ->
                                                Nothing

                                            Matured return ->
                                                Just return
                                    )
                        )
                    |> Dict.toList
                    |> List.concatMap
                        (\( pair, innerDict ) ->
                            innerDict
                                |> Dict.toList
                                |> List.map
                                    (\( maturity, return ) ->
                                        { pair = pair
                                        , maturity = maturity
                                        , return = return
                                        }
                                    )
                        )
            )



-- toDueList : Positions -> List DuesInfo
-- toDueList (Positions { dues }) =
--     dues
--         |> Dict.toList
--         |> List.concatMap
--             (\( pair, innerDict ) ->
--                 innerDict
--                     |> Dict.toList
--                     |> List.map
--                         (\( maturity, veryInnerDict ) ->
--                             { pair = pair
--                             , maturity = maturity
--                             , listDue =
--                                 veryInnerDict
--                                     |> Dict.toList
--                                     |> List.map
--                                         (\( tokenId, { debt, collateral } ) ->
--                                             { tokenId = tokenId
--                                             , debt = debt
--                                             , collateral = collateral
--                                             }
--                                         )
--                             }
--                         )
--             )


example : Positions
example =
    { liquidities = Dict.empty (Pair.sorter Rinkeby)
    , claims =
        Dict.fromList (Pair.sorter Rinkeby)
            [ ( Pair.daiEthRinkeby
              , Dict.fromList Maturity.sorter
                    [ ( Maturity.unix962654400
                      , Matured { asset = "1200", collateral = "3.56" }
                      )
                    , ( Maturity.unix1635364800
                      , Active { bond = "1200", insurance = "2.0008" }
                      )
                    , ( Maturity.unix1650889815
                      , Matured { asset = "45", collateral = "3.2" }
                      )
                    ]
              )
            , ( Pair.daiMaticRinkeby
              , Dict.fromList Maturity.sorter
                    [ ( Maturity.unix962654400
                      , Active { bond = "2133", insurance = "21313" }
                      )
                    ]
              )
            ]
    , dues = Dict.empty (Pair.sorter Rinkeby)
    }
        |> Positions
