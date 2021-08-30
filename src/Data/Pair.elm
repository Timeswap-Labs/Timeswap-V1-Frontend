module Data.Pair exposing (Pair, daiEthRinkeby, daiMaticRinkeby, fromFragment, sorter, toAddress, toAsset, toCollateral, toFragment, toKey, wethDaiRinkeby, whitelist)

import Data.Address as Address exposing (Address)
import Data.Chain exposing (Chain(..))
import Data.ERC20 as ERC20
import Data.Token as Token exposing (Token)
import Sort exposing (Sorter)
import Sort.Set as Set exposing (Set)


type Pair
    = Pair
        { address : Address
        , asset : Token
        , collateral : Token
        }


fromFragment : Chain -> String -> String -> Maybe Pair
fromFragment chain assetString collateralString =
    Maybe.map2
        (\asset collateral ->
            whitelist chain
                |> Set.foldl
                    (\pair accumulator ->
                        if (pair |> toAsset) == asset && (pair |> toCollateral) == collateral then
                            Just pair

                        else
                            accumulator
                    )
                    Nothing
        )
        (assetString |> Token.fromAssetFragment chain)
        (collateralString |> Token.fromCollateralFragment chain)
        |> Maybe.andThen identity


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


toAddress : Pair -> Address
toAddress (Pair { address }) =
    address


toAsset : Pair -> Token
toAsset (Pair { asset }) =
    asset


toCollateral : Pair -> Token
toCollateral (Pair { collateral }) =
    collateral


toKey : Pair -> String
toKey pair =
    pair
        |> toAddress
        |> Address.toString


whitelist : Chain -> Set Pair
whitelist chain =
    (case chain of
        Mainnet ->
            []

        Rinkeby ->
            [ daiEthRinkeby
            , daiMaticRinkeby
            , wethDaiRinkeby
            ]
    )
        |> Set.fromList (sorter chain)


toRank : Chain -> Pair -> Int
toRank chain pair =
    case chain of
        Mainnet ->
            0

        Rinkeby ->
            if pair == daiEthRinkeby then
                0

            else if pair == daiMaticRinkeby then
                1

            else if pair == wethDaiRinkeby then
                2

            else
                3


compare : Chain -> Pair -> Pair -> Order
compare chain ((Pair pair1) as pairA) ((Pair pair2) as pairB) =
    Basics.compare (pairA |> toRank chain) (pairB |> toRank chain)
        |> (\order ->
                case order of
                    EQ ->
                        Token.compare chain pair1.asset pair2.asset
                            |> (\orderAsset ->
                                    case orderAsset of
                                        EQ ->
                                            Token.compare chain pair1.collateral pair2.collateral

                                        _ ->
                                            orderAsset
                               )

                    _ ->
                        order
           )


sorter : Chain -> Sorter Pair
sorter chain =
    Sort.custom (compare chain)


daiEthRinkeby : Pair
daiEthRinkeby =
    Pair
        { address = Address.daiWethRinkeby
        , asset = Token.ERC20 ERC20.daiRinkeby
        , collateral = Token.ETH
        }


daiMaticRinkeby : Pair
daiMaticRinkeby =
    Pair
        { address = Address.daiMaticRinkeby
        , asset = Token.ERC20 ERC20.daiRinkeby
        , collateral = Token.ERC20 ERC20.maticRinkeby
        }


wethDaiRinkeby : Pair
wethDaiRinkeby =
    Pair
        { address = Address.wethDaiRinkeby
        , asset = Token.ERC20 ERC20.wethRinkeby
        , collateral = Token.ERC20 ERC20.daiRinkeby
        }
