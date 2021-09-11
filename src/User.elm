module User exposing
    ( Error(..)
    , User
    , decoder
    , errorToMessage
    , same
    , toChain
    , updateAllowances
    , updateBalances
    , updatePositions
    )

import Data.Address as Address exposing (Address)
import Data.Allowances as Allowances exposing (Allowances)
import Data.Balances as Balances exposing (Balances)
import Data.Chain as Chain exposing (Chain(..))
import Data.Pools exposing (Pools)
import Data.Positions as Positions exposing (Positions)
import Data.Remote exposing (Remote(..))
import Data.Tokens exposing (Tokens)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline


type alias User =
    { chain : Chain
    , address : Address
    , positions : Remote () Positions
    , balances : Remote () Balances
    , allowances : Remote () Allowances
    }


type Error
    = UnsupportedNetwork


decoder : Decoder (Remote Error User)
decoder =
    Decode.succeed
        (\result address ->
            case result of
                Ok chain ->
                    { chain = chain
                    , address = address
                    , positions = Loading
                    , balances = Loading
                    , allowances = Loading
                    }
                        |> Success

                Err _ ->
                    Failure UnsupportedNetwork
        )
        |> Pipeline.required "chainId" Chain.decoder
        |> Pipeline.required "user" Address.decoder


same : User -> User -> Bool
same user1 user2 =
    user1.chain == user2.chain && user1.address == user2.address


toChain : Remote userError { user | chain : Chain } -> Chain
toChain user =
    case user of
        Success { chain } ->
            chain

        _ ->
            Rinkeby


updatePositions : Pools -> Tokens -> Value -> User -> User
updatePositions pools tokens value user =
    { user
        | positions =
            case user.positions of
                Success positions ->
                    positions
                        |> Positions.update pools tokens value
                        |> Success

                _ ->
                    Positions.init pools tokens value
    }


updateBalances : Tokens -> Value -> User -> User
updateBalances tokens value user =
    { user
        | balances =
            case user.balances of
                Success balances ->
                    balances
                        |> Balances.update tokens value
                        |> Success

                _ ->
                    Balances.init tokens value
    }


updateAllowances : Tokens -> Value -> User -> User
updateAllowances tokens value user =
    { user
        | allowances =
            case user.allowances of
                Success allowances ->
                    allowances
                        |> Allowances.update tokens value
                        |> Success

                _ ->
                    Allowances.init tokens value
    }


errorToMessage : Error -> String
errorToMessage error =
    case error of
        UnsupportedNetwork ->
            "Wrong network, must connect to Rinkeby network."
