module Services.Swap.GameToken exposing (GameToken(..), encode, toApiTokenId, toERC20, toToken)

import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Token as Token exposing (Token)
import Json.Encode as Encode exposing (Value)


type GameToken
    = Matic
    | Axie
    | Shiba
    | Doge
    | USDC
    | ETH


encode : GameToken -> Value
encode gameToken =
    gameToken
        |> toApiTokenId
        |> Encode.string


toApiTokenId : GameToken -> String
toApiTokenId gameToken =
    case gameToken of
        Matic ->
            "matic-network"

        Axie ->
            "axie-infinity"

        Shiba ->
            "shiba-inu"

        Doge ->
            "dogecoin"

        USDC ->
            "usd-coin"

        ETH ->
            "ethereum"


toERC20 : GameToken -> ERC20
toERC20 gameToken =
    case gameToken of
        Matic ->
            ERC20.matic

        Axie ->
            ERC20.axie

        Shiba ->
            ERC20.shiba

        Doge ->
            ERC20.doge

        USDC ->
            ERC20.usdc

        ETH ->
            ERC20.eth


toToken : GameToken -> Token
toToken gameToken =
    gameToken |> toERC20 |> Token.ERC20
