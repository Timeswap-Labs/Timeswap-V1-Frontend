module Blockchain.User.Positions exposing
    ( Positions
    , dummy
    , subscriptions
    , update
    )

import Blockchain.User.Claims as Claims exposing (Claims)
import Blockchain.User.Dues exposing (Dues)
import Blockchain.User.Liquidities as Liquidities exposing (Liquidities)
import Data.Pool as Pool
import Sort.Dict as Dict
import Time exposing (Posix)


type alias Positions =
    { claims : Claims
    , dues : Dues
    , liquidities : Liquidities
    }


dummy : Positions
dummy =
    { claims = Dict.empty Pool.sorter
    , dues = Dict.empty Pool.sorter
    , liquidities = Dict.empty Pool.sorter
    }


update : Posix -> Positions -> Positions
update posix ({ claims, liquidities } as positions) =
    { positions
        | claims = claims |> Claims.update posix
        , liquidities = liquidities |> Liquidities.update posix
    }


subscriptions : (Posix -> msg) -> Positions -> Sub msg
subscriptions tick { claims, liquidities } =
    [ claims |> Claims.subscriptions tick
    , liquidities |> Liquidities.subscriptions tick
    ]
        |> Sub.batch
