module Data.Pair exposing
    ( Pair
    , daiEthRinkeby
    , daiMaticRinkeby
    , example
    , sorter
    , toAsset
    , toCollateral
    , toFragment
    , toKey
    , wethDaiRinkeby
    )

import Data.Chain exposing (Chain(..))
import Data.ERC20 as ERC20
import Data.Token as Token exposing (Token)
import Sort exposing (Sorter)
import Sort.Set as Set exposing (Set)


type Pair
    = Pair
        { id : Int
        , asset : Token
        , collateral : Token
        }


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


example : Set Pair
example =
    Set.fromList sorter
        [ daiEthRinkeby
        , daiMaticRinkeby
        , wethDaiRinkeby
        ]


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
