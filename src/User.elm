module User exposing (User, decoder, same, toChain)

import Data.Address as Address exposing (Address)
import Data.Allowances as Allowances exposing (Allowances)
import Data.Balances as Balances exposing (Balances)
import Data.Chain as Chain exposing (Chain(..))
import Data.Positions as Positions exposing (Positions)
import Data.Remote exposing (Remote(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline


type alias User =
    { chain : Chain
    , address : Address
    , positions : Remote Positions
    , balances : Remote Balances
    , allowances : Remote Allowances
    }


decoder : Decoder User
decoder =
    Decode.succeed User
        |> Pipeline.required "chainId" Chain.decoder
        |> Pipeline.required "user" Address.decoder
        |> Pipeline.custom (Decode.succeed (Success Positions.example))
        |> Pipeline.custom (Decode.succeed (Success Balances.example))
        |> Pipeline.custom (Decode.succeed (Success Allowances.example))


same : User -> User -> Bool
same user1 user2 =
    user1.chain == user2.chain && user1.address == user2.address


toChain : Maybe { user | chain : Chain } -> Chain
toChain user =
    user
        |> Maybe.map .chain
        |> Maybe.withDefault Rinkeby
