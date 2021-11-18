module Data.Pools exposing
    ( PoolInfo
    , Pools
    , decoder
    , decoderPool
    , empty
    , fromPairFragment
    , fromPoolFragment
    , getCollateralizedDebt
    , getFirst
    , toList
    , toListSinglePair
    , toPairs
    , update
    )

import Data.Address as Address exposing (Address)
import Data.Chain exposing (Chain(..))
import Data.Maturity as Maturity exposing (Maturity)
import Data.Pair as Pair exposing (Pair)
import Data.Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Tokens as Tokens exposing (Tokens)
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline
import Sort.Dict as Dict exposing (Dict)
import Time exposing (Posix)


type Pools
    = Pools (Dict Pair (Dict Maturity ( Native, Remote () PoolUint ))) -- fix in the future


type alias Native =
    { liquidity : Address
    , bond : Address
    , insurance : Address
    , collateralizedDebt : Address
    }


type alias PoolUint =
    { assetLiquidity : Uint
    , collateralLiquidity : Uint
    , apr : Float
    , cf : Uint
    }


type alias PoolInfo =
    { assetLiquidity : String
    , collateralLiquidity : String
    , apr : String
    , cf : String
    }


decoder : Tokens -> Decoder Pools
decoder tokens =
    let
        recursive :
            Int
            -> Value
            -> ( Int, List ( Pair, List ( Maturity, Native ) ) )
            -> Decoder (List ( Pair, List ( Maturity, Native ) ))
        recursive length value ( id, list ) =
            if id < length then
                value
                    |> Decode.decodeValue
                        (Decode.succeed Tuple.pair
                            |> Pipeline.custom (Pair.decoder tokens id)
                            |> Pipeline.required "pools"
                                (Decode.succeed Tuple.pair
                                    |> Pipeline.required "maturity" Maturity.decoder
                                    |> Pipeline.custom
                                        (Decode.succeed Native
                                            |> Pipeline.required "liquidity" Address.decoder
                                            |> Pipeline.required "bond" Address.decoder
                                            |> Pipeline.required "insurance" Address.decoder
                                            |> Pipeline.required "collateralizedDebt" Address.decoder
                                        )
                                    |> Decode.list
                                )
                            |> Decode.index id
                        )
                    |> (\result ->
                            case result of
                                Ok pools ->
                                    ( id + 1, pools :: list )
                                        |> recursive length value

                                Err error ->
                                    error
                                        |> Decode.errorToString
                                        |> Decode.fail
                       )

            else
                Decode.succeed list
    in
    Decode.succeed (\length value -> recursive length value ( 0, [] ))
        |> Pipeline.custom
            (Decode.succeed ()
                |> Decode.list
                |> Decode.map List.length
            )
        |> Pipeline.custom Decode.value
        |> Decode.andThen identity
        |> Decode.map
            (\list ->
                list
                    |> (List.map << Tuple.mapSecond)
                        (\innerList ->
                            innerList
                                |> List.map (\( maturity, native ) -> ( maturity, ( native, Loading ) ))
                                |> Dict.fromList Maturity.sorter
                        )
                    |> Dict.fromList Pair.sorter
                    |> Pools
            )


decoderPool : Pools -> Tokens -> Decoder Pool
decoderPool (Pools dict) tokens =
    Decode.succeed
        (\asset collateral maturity ->
            dict
                |> Dict.foldl
                    (\pair innerDict accumulator ->
                        if
                            ((pair |> Pair.toAsset) == asset)
                                && ((pair |> Pair.toCollateral) == collateral)
                                && (maturity |> Dict.memberOf innerDict)
                        then
                            { pair = pair
                            , maturity = maturity
                            }
                                |> Decode.succeed

                        else
                            accumulator
                    )
                    (Decode.fail "not in the whitelist")
        )
        |> Pipeline.required "asset" (Tokens.decoderToken tokens)
        |> Pipeline.required "collateral" (Tokens.decoderToken tokens)
        |> Pipeline.required "maturity" Maturity.decoder
        |> Decode.andThen identity


decoderPoolUint : Decoder PoolUint
decoderPoolUint =
    Decode.succeed PoolUint
        |> Pipeline.required "assetLiquidity" Uint.decoder
        |> Pipeline.required "collateralLiquidity" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cf" Uint.decoder


decoderUpdate : Pools -> Tokens -> Decoder Pools
decoderUpdate ((Pools dict) as pools) tokens =
    Decode.succeed Tuple.pair
        |> Pipeline.custom (decoderPool pools tokens)
        |> Pipeline.custom decoderPoolUint
        |> Decode.list
        |> Decode.map
            (\list ->
                list
                    |> List.foldl
                        (\( { pair, maturity }, poolInfo ) accumulator ->
                            accumulator
                                |> Dict.get pair
                                |> Maybe.map
                                    (\innerDict ->
                                        innerDict
                                            |> Dict.get maturity
                                            |> Maybe.map
                                                (\( native, _ ) ->
                                                    innerDict
                                                        |> Dict.insert maturity ( native, Success poolInfo )
                                                )
                                            |> Maybe.withDefault innerDict
                                            |> (\newDict ->
                                                    accumulator
                                                        |> Dict.insert pair newDict
                                               )
                                    )
                                |> Maybe.withDefault accumulator
                        )
                        dict
                    |> Pools
            )


update : Tokens -> Value -> Pools -> Pools
update tokens value ((Pools dict) as pools) =
    value
        |> Decode.decodeValue (decoderUpdate pools tokens)
        |> (\result ->
                case result of
                    Ok (Pools newDict) ->
                        newDict
                            |> Dict.foldl
                                (\pair newInnerDict accumulator ->
                                    accumulator
                                        |> Dict.insert pair
                                            (accumulator
                                                |> Dict.get pair
                                                |> Maybe.map
                                                    (\innerDict ->
                                                        innerDict
                                                            |> Dict.insertAll newInnerDict
                                                    )
                                                |> Maybe.withDefault newInnerDict
                                            )
                                )
                                dict
                            |> Pools

                    Err _ ->
                        Pools dict
           )


empty : Pools
empty =
    Dict.empty Pair.sorter
        |> Pools


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


toPairs : Pools -> List Pair
toPairs (Pools dict) =
    dict |> Dict.keys


toAPR : Float -> String
toAPR float =
    float
        |> (*) 10000
        |> String.fromFloat
        |> String.padLeft 3 '0'
        |> String.split "."
        |> List.head
        |> Maybe.map
            (\string ->
                [ string |> String.dropRight 2
                , string |> String.right 2
                ]
                    |> String.join "."
            )
        |> Maybe.withDefault ""


toList : Posix -> Pools -> List ( Pair, List ( Maturity, Remote () PoolInfo ) )
toList time (Pools dict) =
    dict
        |> Dict.toList
        |> List.map
            (\( pair, innerDict ) ->
                ( pair
                , innerDict
                    |> Dict.keepIf
                        (\maturity _ -> maturity |> Maturity.isActive time)
                    |> Dict.map
                        (\_ ( _, remote ) ->
                            case remote of
                                Success { assetLiquidity, collateralLiquidity, apr, cf } ->
                                    { assetLiquidity =
                                        assetLiquidity
                                            |> Uint.toAmount (pair |> Pair.toAsset)
                                    , collateralLiquidity =
                                        collateralLiquidity
                                            |> Uint.toAmount (pair |> Pair.toCollateral)
                                    , apr = apr |> toAPR
                                    , cf =
                                        cf
                                            |> Uint.toAmount (pair |> Pair.toCollateral)
                                    }
                                        |> Success

                                Loading ->
                                    Loading

                                Failure error ->
                                    Failure error
                        )
                    |> Dict.toList
                )
            )


toListSinglePair : Posix -> Pair -> Pools -> List ( Maturity, Remote () PoolInfo )
toListSinglePair time pair (Pools dict) =
    dict
        |> Dict.get pair
        |> Maybe.map
            (Dict.keepIf (\maturity _ -> maturity |> Maturity.isActive time))
        |> (Maybe.map << Dict.map)
            (\_ ( _, remote ) ->
                case remote of
                    Success { assetLiquidity, collateralLiquidity, apr, cf } ->
                        { assetLiquidity =
                            assetLiquidity
                                |> Uint.toAmount (pair |> Pair.toAsset)
                        , collateralLiquidity =
                            collateralLiquidity
                                |> Uint.toAmount (pair |> Pair.toCollateral)
                        , apr = apr |> toAPR
                        , cf =
                            cf
                                |> Uint.toAmount (pair |> Pair.toCollateral)
                        }
                            |> Success

                    Loading ->
                        Loading

                    Failure error ->
                        Failure error
            )
        |> Maybe.map Dict.toList
        |> Maybe.withDefault []


getFirst : Pools -> Maybe Pair
getFirst (Pools dict) =
    dict
        |> Dict.keys
        |> List.head


getCollateralizedDebt : Pool -> Pools -> Maybe Address
getCollateralizedDebt { pair, maturity } (Pools dict) =
    dict
        |> Dict.get pair
        |> Maybe.andThen (Dict.get maturity)
        |> Maybe.map Tuple.first
        |> Maybe.map .collateralizedDebt
