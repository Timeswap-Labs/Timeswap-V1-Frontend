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
import Data.ERC20 as ERC20
import Data.ERC20s exposing (ERC20s)
import Data.Pool as Pool
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Sort.Dict as Dict
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


decoder : Decoder Answer
decoder =
    Decode.succeed Answer
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "owner" Address.decoder
        |> Pipeline.required "positions" decoderPositions


decoderPositions : Decoder Positions
decoderPositions =
    Decode.succeed Positions
        |> Pipeline.required "claims" Claims.decoder
        |> Pipeline.required "dues" Dues.decoder
        |> Pipeline.required "liqs" Liqs.decoder


toERC20s : Positions -> ERC20s
toERC20s { claims, dues, liqs } =
    claims
        |> Claims.toERC20s
        |> Set.union ERC20.sorter
            (dues |> Dues.toERC20s)
        |> Set.union ERC20.sorter
            (liqs |> Liqs.toERC20s)
