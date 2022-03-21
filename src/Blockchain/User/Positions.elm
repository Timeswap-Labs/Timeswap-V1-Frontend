module Blockchain.User.Positions exposing
    ( Positions
    , decoder
    , toERC20s
    )

import Blockchain.User.Claims as Claims exposing (Claims)
import Blockchain.User.Dues as Dues exposing (Dues)
import Blockchain.User.Liqs as Liqs exposing (Liqs)
import Data.Address as Address exposing (Address)
import Data.Chain as Chain exposing (Chain)
import Data.Chains exposing (Chains)
import Data.ERC20 as ERC20
import Data.ERC20s exposing (ERC20s)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Sort.Set as Set


type alias Positions =
    { claims : Claims
    , dues : Dues
    , liqs : Liqs
    }


type alias Answer =
    { chain : Chain
    , owner : Address
    , positions : Positions
    }


decoder : Chains -> Decoder Answer
decoder chains =
    Decode.field "chain" Chain.decoder
        |> Decode.andThen
            (\chain ->
                Decode.succeed (Answer chain)
                    |> Pipeline.required "owner" Address.decoder
                    |> Pipeline.required "positions" (decoderPositions chain chains)
            )


decoderPositions : Chain -> Chains -> Decoder Positions
decoderPositions chain chains =
    Decode.succeed Positions
        |> Pipeline.required "claims" (Claims.decoder chain chains)
        |> Pipeline.required "dues" (Dues.decoder chain chains)
        |> Pipeline.required "liqs" (Liqs.decoder chain chains)


toERC20s : Positions -> ERC20s
toERC20s { claims, dues, liqs } =
    claims
        |> Claims.toERC20s
        |> Set.union ERC20.sorter
            (dues |> Dues.toERC20s)
        |> Set.union ERC20.sorter
            (liqs |> Liqs.toERC20s)
