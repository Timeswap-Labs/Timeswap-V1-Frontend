module Data.Pair exposing
    ( Pair
    , daiEthRinkeby
    , daiMaticRinkeby
    , decoder
    , sorter
    , toAsset
    , toCollateral
    , toFragment
    , toKey
    , wethDaiRinkeby
    )

import Data.Address as Address
import Data.Chain exposing (Chain(..))
import Data.ERC20 as ERC20
import Data.Token as Token exposing (Token)
import Data.Tokens as Tokens exposing (Tokens)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Sort exposing (Sorter)


type Pair
    = Pair
        { id : Int
        , asset : Token
        , collateral : Token
        }


decoder : Tokens -> Int -> Decoder Pair
decoder tokens id =
    Decode.succeed
        (\asset collateral ->
            if asset == collateral then
                Decode.fail "Cannot be the same tokens"

            else
                { id = id
                , asset = asset
                , collateral = collateral
                }
                    |> Pair
                    |> Decode.succeed
        )
        |> Pipeline.required "asset"
            (Decode.oneOf
                [ Token.decoderETH
                , Address.decoder
                    |> Decode.andThen
                        (\address ->
                            tokens
                                |> Tokens.getToken address
                                |> Maybe.map Decode.succeed
                                |> Maybe.withDefault (Decode.fail "Not a whitelisted token")
                        )
                ]
            )
        |> Pipeline.required "collateral"
            (Decode.oneOf
                [ Token.decoderETH
                , Address.decoder
                    |> Decode.andThen
                        (\address ->
                            tokens
                                |> Tokens.getToken address
                                |> Maybe.map Decode.succeed
                                |> Maybe.withDefault (Decode.fail "Not a whitelisted token")
                        )
                ]
            )
        |> Decode.andThen identity


toFragment : Pair -> String
toFragment pair =
    [ pair
        |> toAsset
        |> Token.toAssetFragment
    , pair
        |> toCollateral
        |> Token.toCollateralFragment
    ]
        |> String.join "&"


toKey : Pair -> String
toKey (Pair { asset, collateral }) =
    [ asset |> Token.toKey
    , collateral |> Token.toKey
    ]
        |> String.join " "


toAsset : Pair -> Token
toAsset (Pair { asset }) =
    asset


toCollateral : Pair -> Token
toCollateral (Pair { collateral }) =
    collateral


compare : Pair -> Pair -> Order
compare (Pair pair1) (Pair pair2) =
    Basics.compare pair1.id pair2.id


sorter : Sorter Pair
sorter =
    Sort.custom compare


daiEthRinkeby : Pair
daiEthRinkeby =
    Pair
        { id = 0
        , asset = Token.ERC20 ERC20.daiRinkeby
        , collateral = Token.ETH
        }


daiMaticRinkeby : Pair
daiMaticRinkeby =
    Pair
        { id = 1
        , asset = Token.ERC20 ERC20.daiRinkeby
        , collateral = Token.ERC20 ERC20.maticRinkeby
        }


wethDaiRinkeby : Pair
wethDaiRinkeby =
    Pair
        { id = 2
        , asset = Token.ERC20 ERC20.wethRinkeby
        , collateral = Token.ERC20 ERC20.daiRinkeby
        }
