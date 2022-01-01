module Services.Swap.GameToken exposing (GameToken(..), encode, toApiTokenId, toERC20, toToken)

import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Token as Token exposing (Token)
import Json.Encode as Encode exposing (Value)


type GameToken
    = USDC
    | Matic
    | Avalanche
    | ETH


encode : GameToken -> Value
encode gameToken =
    gameToken
        |> toApiTokenId
        |> Encode.string


toApiTokenId : GameToken -> String
toApiTokenId gameToken =
    case gameToken of
        USDC ->
            "usd-coin"

        Matic ->
            "matic-network"

        Avalanche ->
            "avalanche-2"

        ETH ->
            "ethereum"


toERC20 : GameToken -> ERC20
toERC20 gameToken =
    case gameToken of
        USDC ->
            ERC20.usdc

        Matic ->
            ERC20.matic

        Avalanche ->
            ERC20.avalanche

        ETH ->
            ERC20.eth


toToken : GameToken -> Token
toToken gameToken =
    gameToken |> toERC20 |> Token.ERC20
