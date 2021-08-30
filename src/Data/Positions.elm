module Data.Positions exposing (ClaimsInfo, Positions, toListClaims)

import Data.Chain exposing (Chain(..))
import Data.Maturity as Maturity exposing (Maturity)
import Data.Pair as Pair exposing (Pair)
import Data.Remote as Remote exposing (Remote(..))
import Data.TokenId exposing (TokenId)
import Sort.Dict as Dict exposing (Dict)
import Time exposing (Posix)


type Positions
    = Positions (Dict Pair (Dict Maturity Position))


type alias Position =
    { liquidity : String
    , bond : String
    , insurance : String
    , collateralizedDebt : Dict TokenId CollateralizedDebt
    }


type alias CollateralizedDebt =
    { debt : String
    , collateral : String
    }


type alias LiquidityInfo =
    { maturity : Maturity
    , liquidity : String
    }


type alias ClaimsInfo =
    { pair : Pair
    , maturity : Maturity
    , bond : String
    , insurance : String
    }


type alias DuesInfo =
    { maturity : Maturity
    , dues : List CollateralizedDebtInfo
    }


type alias CollateralizedDebtInfo =
    { tokenId : TokenId
    , debt : String
    , collateral : String
    }


toListClaims : Positions -> List ClaimsInfo
toListClaims (Positions dict) =
    dict
        |> Dict.toList
        |> List.concatMap
            (\( pair, innerDict ) ->
                innerDict
                    |> Dict.toList
                    |> List.map
                        (\( maturity, { bond, insurance } ) ->
                            { pair = pair
                            , maturity = maturity
                            , bond = bond
                            , insurance = insurance
                            }
                        )
            )


example : Positions
example =
    Dict.fromList (Pair.sorter Rinkeby)
        [ ( Pair.daiEthRinkeby
          , Dict.fromList Maturity.sorter
                []
          )
        ]
        |> Positions
