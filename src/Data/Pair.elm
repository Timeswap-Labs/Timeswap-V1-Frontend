module Data.Pair exposing (Pair, fromFragment, sorter, toAddress, toAsset, toCollateral, toFragment)

import Data.Address exposing (Address)
import Data.Chain exposing (Chain)
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


whitelist : Chain -> Set Pair
whitelist chain =
    Set.empty (sorter chain)


sorter : Chain -> Sorter Pair
sorter chain =
    Sort.increasing
        |> Sort.by (\pair -> 0)
        |> Sort.tiebreaker (Token.sorter chain |> Sort.by (\(Pair { asset }) -> asset))
        |> Sort.tiebreaker (Token.sorter chain |> Sort.by (\(Pair { collateral }) -> collateral))
